{-# LANGUAGE TemplateHaskell #-}
module Network.Salvia.Handler.Session where

import Control.Applicative hiding (empty)
import Control.Category
import Control.Concurrent.STM hiding (check)
import Control.Monad.State hiding (get, sequence)
import Data.List
import Data.Maybe
import Data.Record.Label
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Traversable
import Network.Protocol.Cookie hiding (empty)
import Network.Protocol.Http hiding (cookie)
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Cookie
import Prelude hiding ((.), id, lookup, sequence)
import Safe
import System.Random
import qualified Control.Monad.State as S
import qualified Data.Map as M

-- | A session identifier. Should be unique for every session.

newtype SessionID = SID { _sid :: Integer }
  deriving (Eq, Ord, Random)

$(mkLabels [''SessionID])

sid :: SessionID :-> Integer

instance Show SessionID where
  show = show . get sid

-- | The session data type with polymorphic payload.

data Session p =
  Session
  { _sID      :: SessionID
  , _sStart   :: UTCTime
  , _sLast    :: UTCTime
  , _sExpire  :: Integer
  , _sPayload :: Maybe p
  } deriving Show

$(mkLabels [''Session])

sID      :: Session p :-> SessionID  -- ^ A globally unique session identifier.
sStart   :: Session p :-> UTCTime    -- ^ The time the session started.
sLast    :: Session p :-> UTCTime    -- ^ Last time session was used.
sExpire  :: Session p :-> Integer    -- ^ Expire after this amount of time when unused.
sPayload :: Session p :-> Maybe p    -- ^ The information this session stores.

-- | Session type classes that hide the inner workings from the outside world.

class (Applicative m, Monad m) => SessionM m p | m -> p where
  prolongSession :: Integer -> m ()
  getSession     :: m (Session p)
  putSession     :: Session p -> m ()
  delSession     :: m ()
  withSession    :: (Session p -> Session p) -> m ()

-- | A mapping from unique session IDs to shared session variables.

type Sessions p = TVar (M.Map SessionID (TVar (Session p)))

-- | Create a new, empty, store of sessions.

mkSessions :: MonadIO m => m (Sessions p)
mkSessions = newVar M.empty

{- | todo doc:
The session handler. This handler will try to return an existing session from
the sessions map based on a session identifier found in the HTTP `cookie`. When
such a session can be found the expiration date will be updated to a number of
seconds in the future. When no session can be found a new one will be created.
A cookie will be set that informs the client of the current session.
-}

hProlongSession :: (MonadIO m, HttpM' m, ServerM m, PayloadM m (Sessions p)) => Integer -> m ()
hProlongSession e =
  do n <- liftIO getCurrentTime
     var <- existingSessionVarOrNew
     session <- modVar (set sLast n . set sExpire e) var >>= getVar
     setCookieSession (get sID session) (willExpireAt session)
     return ()

hGetSession :: (MonadIO m, HttpM' m, ServerM m, PayloadM m (Sessions p)) => m (Session p)
hGetSession = existingSessionVarOrNew >>= getVar

hPutSession :: (MonadIO m, HttpM' m, ServerM m, PayloadM m (Sessions p)) => Session p -> m ()
hPutSession session =
  do var <- existingSessionVarOrNew
     putVar var session
     setCookieSession (get sID session) (willExpireAt session)

hDelSession :: (PayloadM m (Sessions p), HttpM' m, MonadIO m) => m ()
hDelSession =
  do msid <- getCookieSessionID
     case msid of
       Just sd ->
         do delCookieSession
            var <- payload S.get
            _   <- modVar (M.delete sd) var
            return ()
       Nothing -> return ()

hWithSession
  :: (PayloadM m (Sessions p), MonadIO m, HttpM Request m)
  => (Session p -> Session p) -> m ()
hWithSession f =
  do _ <- existingSessionVarOrNew >>= modVar f
     return ()

hSessionInfo :: (SessionM m p, SendM m) => m ()
hSessionInfo =
  do s <- getSession
     (send . intercalate "\n")
       [ "id="      ++ show (get sID     s)
       , "start="   ++ show (get sStart  s)
       , "last="    ++ show (get sLast   s)
       , "expire="  ++ show (get sExpire s)
       , "payload=" ++ show (isJust $ get sPayload s)
       ]

{- | todo:
Given an existing session identifier lookup a session from the session map.
When no session is available, or the session is expired, create a new one using
the `newSessionVar' function. Otherwise the expiration date of the existing
session is updated.
-}

existingSessionVarOrNew :: (Applicative m, MonadIO m, HttpM Request m, PayloadM m (Sessions p)) => m (TVar (Session p))
existingSessionVarOrNew =
  getCookieSessionID `andAlso` lookupSessionVar `andAlso` whenNotExpired >>=
     (newSessionVar `maybe` return)
  where andAlso m c = m >>= liftM join . sequence . fmap c

whenNotExpired :: MonadIO m => TVar (Session p) -> m (Maybe (TVar (Session p)))
whenNotExpired var =
  do n <- liftIO getCurrentTime
     session <- getVar var
     return $ if (willExpireAt session > n) then Just var else Nothing

{- |
This handler sets the HTTP cookie for the specified session. It will use a
default cookie with an additional `sid' attribute with the session identifier
as value. The session expiration date will be used as the cookie expire field.
-}

setCookieSession :: (MonadIO m, ServerM m, HttpM Response m) => SessionID -> UTCTime -> m ()
setCookieSession sd ex =
  do zone <- liftIO getCurrentTimeZone
     let time = utcToLocalTime zone ex
     ck <- set name "sid" . set value (show sd) <$> hNewCookie time
     hSetCookie (fromList [ck])

-- | Given the (possibly wrong) request cookie, try to recover the existing
-- session identifier. When there is none, create a new one.

getCookieSessionID :: (MonadIO m, HttpM Request m) => m (Maybe SessionID)
getCookieSessionID =
  fmap SID . join . fmap readMay . join . fmap (get (fmapL value . pickCookie "sid"))
     <$> hCookie

delCookieSession :: HttpM Response m => m ()
delCookieSession = hDelCookie "sid"

-- | Create a new session with a specified expiration date. The session will be
-- stored in the session map.

newSessionVar :: (MonadIO m, PayloadM m (Sessions p)) => m (TVar (Session p))
newSessionVar =
  do var <- payload S.get
     sd <- newSessionID var
     session <- liftIO getCurrentTime >>= newVar . (\n -> Session sd n n 0 Nothing)
     _ <- modVar (M.insert sd session) var
     return session

newSessionID :: MonadIO m => Sessions p -> m SessionID
newSessionID var =
  do store <- getVar var
     let generate =
           do r <- liftIO randomIO
              case M.lookup r store of
                Nothing -> return r
                Just _  -> generate
     generate

willExpireAt :: Session p -> UTCTime
willExpireAt session = fromInteger (get sExpire session) `addUTCTime` get sLast session 

lookupSessionVar :: (MonadIO m, PayloadM m (Sessions p)) => SessionID -> m (Maybe (TVar (Session p)))
lookupSessionVar sd = M.lookup sd <$> (payload S.get >>= getVar)

-- STM utilities.

newVar :: MonadIO m => a -> m (TVar a)
getVar :: MonadIO m => TVar a -> m a
putVar :: MonadIO m => TVar a -> a -> m ()
modVar :: MonadIO m => (a -> a) -> TVar a -> m (TVar a)

newVar       = liftIO . atomically . newTVar
getVar       = liftIO . atomically . readTVar
putVar   var = liftIO . atomically . writeTVar var
modVar f var = liftIO . atomically $ (readTVar var >>= writeTVar var . f) >> return var

