{-# LANGUAGE TypeSynonymInstances #-}

module Misc.Misc (
    eitherToMaybe
  , bool
  , pMaybe
  , (@@)
  , safeLast
  , guardMaybe
  , ifM
  , split
  , withReverse
  , intersperseS
  , trim
  , normalCase
  , safeRead
  , now
  , later
  , safeHead
  , atomModTVar
  , atomReadTVar
  , atomWithTVar
  ) where

import Control.Applicative 
import Control.Concurrent.STM
import Control.Monad
import Data.Char
import Data.Time.Clock
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-------[ common utilities ]----------------------------------------------------

bool :: a -> a -> Bool -> a
bool a b c = if c then a else b

safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

-- Conversions.

guardMaybe :: Bool -> Maybe ()
guardMaybe = bool (Just ()) Nothing

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-------[ list utilities ]------------------------------------------------------

safeList :: ([a] -> b) -> [a] -> Maybe b
safeList _ [] = Nothing
safeList f xs = Just $ f xs

safeHead :: [a] -> Maybe a
safeHead = safeList head

safeLast :: [a] -> Maybe a
safeLast = safeList last

withReverse :: ([a] -> [b]) -> [a] -> [b]
withReverse f = reverse . f . reverse

trimWith :: (a -> Bool) -> [a] -> [a]
trimWith f = withReverse (dropWhile f) . dropWhile f

split :: Eq a => a -> [a] -> [[a]]
split c = splitWith (==c)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs =
  case span (not . f) (dropWhile f xs) of
   ([], []) -> []
   (a,  []) -> [a]
   (a,  b)  -> a : splitWith f b

-------[ text manipulation ]---------------------------------------------------

normalCase :: String -> String
normalCase ""     = ""
normalCase (x:xs) = toUpper x : map toLower xs

-- Trim all heading and trailing whitespace.
trim :: String -> String
trim = trimWith (`elem` " \t\n\r")

-- ShowS functions.

intersperseS :: ShowS -> [ShowS] -> ShowS
intersperseS _ []  = id
intersperseS _ [s] = s
intersperseS c xs  = foldl1 (\a -> ((a.c).)) xs

-------[ second arity functor ]------------------------------------------------

class Functor2 f where
  fmap2 :: (a -> c) -> (b -> d) -> f a b -> f c d

instance Functor2 (,) where
  fmap2 f g (a, b) = (f a, g b)

instance Functor2 Either where
  fmap2 f _ (Left  a) = Left  (f a)
  fmap2 _ g (Right b) = Right (g b)

-------[ monadic expressions ]-------------------------------------------------

ifM :: Monad m => m Bool -> (a -> a) -> a -> m a
ifM t f g = t >>= (\b -> return $ if b then f g else g)

-------[ parsec extensions ]---------------------------------------------------

-- Helper function to quickly apply a parser.
(@@) :: GenParser Char () a -> String -> Maybe a
(@@) p b = either (const Nothing) Just $ parse (p <* eof) "" b

-- Option parser with maybe result.
pMaybe :: GenParser a b c -> GenParser a b (Maybe c)
pMaybe = option Nothing . liftM Just

-- Make parsec both applicative and alternative.
{-instance Applicative (GenParser s a) where
  pure  = return
  (<*>) = ap

instance Alternative (GenParser s a) where
  empty = mzero
  (<|>) = mplus-}

-------[ time utils ]----------------------------------------------------------

later :: Integer -> IO LocalTime
later howlong = do
  zone <- getCurrentTimeZone
  time <- liftM (addUTCTime $ fromInteger howlong) getCurrentTime
  return $ utcToLocalTime zone time

now :: IO LocalTime
now = later 0

-------[ concurrency utils ]---------------------------------------------------

atomModTVar :: (a -> a) -> TVar a -> IO a
atomModTVar f v = atomically $ do
  t <- readTVar v
  writeTVar v (f t)
  return (f t)

atomReadTVar :: TVar a -> IO a
atomReadTVar = atomically . readTVar

atomWithTVar :: (a -> b) -> TVar a -> IO b
atomWithTVar f v = atomically
  $ liftM f (readTVar v)
  
