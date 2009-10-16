module Network.Salvia.Handler.Session where

import Control.Applicative hiding (empty)
import Control.Concurrent.STM hiding (check)
import Control.Monad.State hiding (get, sequence)
import Data.Record.Label
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Traversable
import Network.Protocol.Cookie hiding (empty)
import Network.Protocol.Http hiding (cookie)
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Cookie
import Safe
import System.Random
import qualified Control.Monad.State as S
import qualified Data.Map as M
import Prelude hiding ((.), id, lookup, sequence)
import Control.Category

-- | Session type classes that hide the inner workings from the outside world.

class SessionM m p where
  useSession  :: Integer -> m (Session p)
  putSession  :: Session p -> m ()
  delSession  :: p -> m ()
--   withSession :: (Session p -> Session p) -> m ()

{- | A session identifier. Should be unique for every session. -}

newtype SessionID = SID { unSID :: Integer }
  deriving (Eq, Ord, Random)

instance Show SessionID where
  show = show . unSID

{- | The session data type with polymorphic payload. -}

data Session p =
  Session
  { sID      :: SessionID  -- ^ A globally unique session identifier.
  , sStart   :: UTCTime    -- ^ The time the session started.
  , sLast    :: UTCTime    -- ^ Last time session was used.
  , sExpire  :: Integer    -- ^ Expire after this amount of time when unused.
  , sPayload :: Maybe p    -- ^ The information this session stores.
  } deriving Show

{- | A shared session. -}

type TSession p = TVar (Session p)

{- | A mapping from unique session IDs to shared session variables. -}

type Sessions p = TVar (M.Map SessionID (TSession p))

{- | Create a new, empty, store of sessions. -}

mkSessions :: MonadIO m => m (Sessions p)
mkSessions = newVar M.empty

{- | todo doc:
The session handler. This handler will try to return an existing session from
the sessions map based on a session identifier found in the HTTP `cookie`. When
such a session can be found the expiration date will be updated to a number of
seconds in the future. When no session can be found a new one will be created.
A cookie will be set that informs the client of the current session.
-}

hUseSession :: (MonadIO m, HttpM' m, ServerM m, PayloadM m (Sessions p)) => Integer -> m (Session p)
hUseSession e =
  do n <- liftIO getCurrentTime
     var <- existingSessionVarOrNew
     session <- modVar (\s -> s {sLast = n, sExpire = e}) var >>= getVar
     setCookieSession (sID session) (willExpireAt session)
     return session

hPutSession :: (MonadIO m, HttpM' m, ServerM m, PayloadM m (Sessions p)) => Session p -> m ()
hPutSession session =
  do var <- existingSessionVarOrNew
     putVar var session
     setCookieSession (sID session) (willExpireAt session)

hDelSession :: forall m p. (PayloadM m (Sessions p), HttpM' m, MonadIO m) => p -> m ()
hDelSession _ =
  do msid <- getCookieSession
     delCookieSession
     case msid of
       Just sid ->
         do var <- payload S.get :: m (Sessions p)
            _ <- modVar (M.delete sid) var
            return ()
       Nothing -> return ()

{- | todo:
Given an existing session identifier lookup a session from the session map.
When no session is available, or the session is expired, create a new one using
the `newSessionVar' function. Otherwise the expiration date of the existing
session is updated.
-}

existingSessionVarOrNew :: (Applicative m, MonadIO m, HttpM Request m, PayloadM m (Sessions p)) => m (TSession p)
existingSessionVarOrNew =
  getCookieSession `andAlso` lookupSessionVar `andAlso` whenNotExpired >>=
    newSessionVar `maybe` return
  where andAlso m c = m >>= liftM join . sequence . fmap c

whenNotExpired :: MonadIO m => TSession p -> m (Maybe (TSession p))
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
setCookieSession sid ex =
  do zone <- liftIO getCurrentTimeZone
     let time = utcToLocalTime zone ex
     ck <- set name "sid" . set value (show sid) <$> hNewCookie time
     hSetCookies (fromList [ck])

{- |
Given the (possibly wrong) request cookie, try to recover the existing session
identifier. When there is none, create a new one.
-}

getCookieSession :: HttpM Request m => m (Maybe SessionID)
getCookieSession =
  fmap SID . join . fmap readMay . join . fmap (get (fmapL value . oneCookie "sid"))
    <$> hGetCookies
  
delCookieSession :: HttpM Response m => m ()
delCookieSession = hDelCookie "sid"

{- |
Create a new session with a specified expiration date. The session will be
stored in the session map.
-}

newSessionVar :: (MonadIO m, PayloadM m (Sessions p)) => m (TSession p)
newSessionVar =
  do var <- payload S.get
     sid <- newSessionID var
     session <- liftIO getCurrentTime >>= newVar . (\n -> Session sid n n 0 Nothing)
     _ <- modVar (M.insert sid session) var
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
willExpireAt session = fromInteger (sExpire session) `addUTCTime` sLast session 

lookupSessionVar :: (MonadIO m, PayloadM m (Sessions p)) => SessionID -> m (Maybe (TSession p))
lookupSessionVar sid = M.lookup sid <$> (payload S.get >>= getVar)

-- STM utilities.

newVar :: MonadIO m => a -> m (TVar a)
getVar :: MonadIO m => TVar a -> m a
putVar :: MonadIO m => TVar a -> a -> m ()
modVar :: MonadIO m => (a -> a) -> TVar a -> m (TVar a)

newVar       = liftIO . atomically . newTVar
getVar       = liftIO . atomically . readTVar
putVar   var = liftIO . atomically . writeTVar var
modVar f var = (liftIO . atomically) (readTVar var >>= writeTVar var . f) >> return var

