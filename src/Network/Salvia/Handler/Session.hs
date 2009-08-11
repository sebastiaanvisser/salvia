{-# LANGUAGE MultiParamTypeClasses #-}
module Network.Salvia.Handler.Session (
    hSession

  , SessionID
  , Session (..)
  , TSession
  , Sessions

  , mkSessions
  ) where

import Control.Applicative hiding (empty)
import Control.Concurrent.STM
import Control.Monad.State
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.LocalTime
import Network.Protocol.Cookie hiding (empty)
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Cookie
import Prelude hiding (lookup)
import Safe
import System.Random
import qualified Data.Map as M

-------------------------------------------------------------------------------

{- | A session identifier. Should be unique for every session. -}

newtype SessionID = SID Integer
  deriving (Eq, Ord)

instance Show SessionID where
  show (SID sid) = show sid

{- | The session data type with polymorphic payload. -}

data Session a = Session {
    sID      :: SessionID  -- ^ A globally unique session identifier.
  , sStart   :: LocalTime  -- ^ The time the session started.
  , sExpire  :: LocalTime  -- ^ The time after which the session is expired.
  , sPayload :: Maybe a    -- ^ The information this session stores.
  } deriving Show

{- | A shared session. -}

type TSession a = TVar (Session a)

{- | Create a new, empty, shared session. -}

mkSession :: SessionID -> LocalTime -> IO (TSession a)
mkSession sid e =
  do s <- now
     atomically (newTVar $ Session sid s e Nothing)

-------------------------------------------------------------------------------

{- | A mapping from unique session IDs to shared session variables. -}

type Sessions a = TVar (M.Map SessionID (TSession a))

{- | Create a new, empty, store of sessions. -}

mkSessions :: IO (Sessions a)
mkSessions = atomically (newTVar M.empty)

-------------------------------------------------------------------------------

{-class (Applicative m, Monad m, MonadIO m) => SessionM m a where
  sessions :: m (Maybe (Sessions a))
  session  :: m (Maybe (Session a))-}

-------------------------------------------------------------------------------

{- |
The session handler. This handler will try to return an existing session from
the sessions map based on a session identifier found in the HTTP `cookie`. When
such a session can be found the expiration date will be updated to a number of
seconds in the future. When no session can be found a new one will be created.
A cookie will be set that informs the client of the current session.
-}

hSession
  :: (MonadIO m, Request m, ServerConfig m, Response m)
  => Sessions a            -- ^ Map of shared session variables.
  -> Integer               -- ^ Number of seconds to be added to the session expiritation time.
  -> m (TSession a)
hSession smap expiration =

  -- Get the session identifier from an existing cookie or create a new one.
  do prev <- getSessionID <$> hGetCookies

     -- Compute current time and expiration time.
     (n, ex) <- liftIO $ liftM2 (,) now (later expiration)

     -- Either create a new session or try to reuse current one.
     tsession <- liftIO $
       maybe
         (newSession smap ex)
         (existingSession smap ex n)
         prev

     setSessionCookie tsession ex
     return tsession

-------------------------------------------------------------------------------

{-
Given the (possible wrong) request cookie, try to recover the existing --
session identifier.
-}

getSessionID :: Maybe Cookies -> Maybe SessionID
getSessionID prev =
  do ck  <- prev
     sid <- cookie "sid" ck
     sid' <- readMay $ value sid
     return (SID sid')

{-
Generate a fresh, random session identifier using the default system random
generator.
-}

genSessionID :: IO SessionID
genSessionID =
  do g <- getStdGen
     let (sid, g') = random g
     setStdGen g'
     return (SID (abs sid))

{-
This handler sets the HTTP cookie for the specified session. It will use a
default cookie with an additional `sid' attribute with the session identifier
as value. The session expiration date will be used as the cookie expire field.
-}

setSessionCookie
  :: (ServerConfig m, FormatTime t, Response m, MonadIO m)
  => TSession a -> t -> m ()
setSessionCookie tsession ex =
  do ck <- newCookie ex
     sid <- liftIO (sID <$> atomically (readTVar tsession))
     hSetCookies $
       cookies [ck {
         name  = "sid"
       , value = show sid
       }]

{-
Handler when no (valid) session is available. Create a new session with a
specified expiration date. The session will be stored in the session map.
-}

newSession :: Sessions a -> LocalTime -> IO (TSession a)
newSession sessions ex =
  do -- Fresh session identifier.
     sid <- genSessionID

     -- Fresh session.
     session <- mkSession sid ex

     -- Place in session mapping usinf session identifier as key.
     atomically (readTVar sessions >>= writeTVar sessions . M.insert sid session)
     return session

{-
TODO: no handler!!!
Handler for existing sessions. Given an existing session identifier lookup a
session from the session map. When no session is available, or the session is
expired, create a new one using the `newSession' function. Otherwise the
expiration date of the existing session is updated.
-}

existingSession :: Sessions a -> LocalTime -> LocalTime -> SessionID -> IO (TSession a)
existingSession sessions ex n sid =

  -- Lookup the session in the session map given the session identifier.
  do mtsession <- M.lookup sid <$> atomically (readTVar sessions)
     case mtsession of

       -- Unrecognized session identifiers are penalised by a fresh session.
       Nothing -> newSession sessions ex
       Just tsession ->
         do expd <- sExpire <$> atomically (readTVar tsession)
            if expd < n

              -- Session is expired, create a new one.
              then newSession sessions ex

              -- Existing session, update expiration date.
              else
                do atomically (readTVar tsession >>= writeTVar tsession . (\s -> s {sExpire = ex}))
                   return tsession

-- Concurrency utils.


-- Time utilities.

later :: Integer -> IO LocalTime
later howlong =
  do zone <- getCurrentTimeZone
     time <- addUTCTime (fromInteger howlong) <$> getCurrentTime
     return $ utcToLocalTime zone time

now :: IO LocalTime
now = later 0

