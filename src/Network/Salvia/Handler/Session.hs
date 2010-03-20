{-# LANGUAGE
    FlexibleContexts
  , TemplateHaskell
  , TypeOperators
  , ScopedTypeVariables
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , FunctionalDependencies
 #-}
module Network.Salvia.Handler.Session where

import Control.Applicative hiding (empty)
import Control.Category
import Control.Concurrent.STM hiding (check)
import Control.Monad.State hiding (get, sequence)
import Control.Monad.Maybe
import Data.List
import Data.Maybe
import Data.Record.Label
import Data.Time.Clock
import Data.Time.LocalTime
import Network.Protocol.Cookie hiding (empty)
import Network.Protocol.Http hiding (cookie)
import Network.Salvia.Interface
import Network.Salvia.Handler.Cookie
import Prelude hiding ((.), id, lookup, sequence, mod)
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
  } deriving (Show)

$(mkLabels [''Session])

-- | A globally unique session identifier.
sID :: Session p :-> SessionID

-- | The time the session started.
sStart :: Session p :-> UTCTime

-- | Last time session was used.
sLast :: Session p :-> UTCTime

-- | Expire after this amount of time when unused.
sExpire :: Session p :-> Integer

-- | The information this session stores.
sPayload :: Session p :-> Maybe p

-- | Session type classes that hides the inner workings from the outside world.

class (Applicative m, Monad m) => SessionM p m | m -> p where
  prolongSession :: Integer -> m ()
  getSession     :: m (Session p)
  putSession     :: Session p -> m ()
  delSession     :: m ()
  withSession    :: (Session p -> Session p) -> m ()

-- | A mapping from unique session IDs to shared session variables.

type SessionMap p = M.Map SessionID (TVar (Session p))
newtype Sessions p = Sessions { unSessions :: SessionMap p }

-- | Apply a function to the sessions in the `Sessions` newtype.

withSessions :: (SessionMap p -> SessionMap p) -> Sessions p -> Sessions p
withSessions f = Sessions . f . unSessions

-- | Create a new, empty, store of sessions.

mkSessions :: Sessions p
mkSessions = Sessions M.empty

{- | todo doc:
The session handler. This handler will try to return an existing session from
the sessions map based on a session identifier found in the HTTP `cookie`. When
such a session can be found the expiration date will be updated to a number of
seconds in the future. When no session can be found a new one will be created.
A cookie will be set that informs the client of the current session.
-}

hProlongSession
  :: forall m q p. (MonadIO m, HttpM' m, ServerM m, ServerAddressM m, PayloadM q (Sessions p) m)
  => p -> Integer -> m ()
hProlongSession _ e =
  do n <- liftIO getCurrentTime
     var <- existingSessionVarOrNew
     session <- modVar (set sLast n . set sExpire e) (var :: TVar (Session p)) >>= getVar
     setCookieSession (get sID session) (willExpireAt session)

hGetSession :: (MonadIO m, HttpM' m, ServerM m, PayloadM q (Sessions p) m) => m (Session p)
hGetSession = existingSessionVarOrNew >>= getVar

hPutSession
  :: (MonadIO m, HttpM' m, ServerM m, ServerAddressM m, PayloadM q (Sessions p) m)
  => Session p -> m ()
hPutSession session =
  do var <- existingSessionVarOrNew
     putVar var session
     setCookieSession (get sID session) (willExpireAt session)

hDelSession :: forall q p m. (PayloadM q (Sessions p) m, HttpM' m, MonadIO m) => p -> m ()
hDelSession _ =
  do msid <- getCookieSessionID
     case msid of
       Just sd ->
         do delCookieSession
            payload . S.modify . withSessions $ (M.delete sd :: SessionMap p -> SessionMap p)
       Nothing -> return ()

hWithSession
  :: (PayloadM q (Sessions p) m, MonadIO m, HttpM Request m)
  => (Session p -> Session p) -> m ()
hWithSession f =
  do _ <- existingSessionVarOrNew >>= modVar f
     return ()

hSessionInfo :: (SessionM p m, SendM m) => m ()
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

existingSessionVarOrNew
  :: (Applicative m, MonadIO m, HttpM Request m, PayloadM q (Sessions p) m)
  => m (TVar (Session p))
existingSessionVarOrNew = fromMaybeTM newSessionVar $
  do sd  <- MaybeT getCookieSessionID
     svar <- MaybeT (lookupSessionVar sd)
     MaybeT (whenNotExpired svar)

whenNotExpired :: MonadIO m => TVar (Session p) -> m (Maybe (TVar (Session p)))
whenNotExpired var =
  do n <- liftIO getCurrentTime
     session <- getVar var
     return $ if (willExpireAt session > n) then Just var else Nothing

{- |
This handler sets the HTTP cookie for the specified session. It will
use a default cookie with an additional `sid' attribute with the
session identifier as value. The session expiration date will be used
as the cookie expire field. The session is valid for all subdomains.
-}

setCookieSession :: (MonadIO m, ServerM m, ServerAddressM m, HttpM Response m) => SessionID -> UTCTime -> m ()
setCookieSession sd ex =
  do zone <- liftIO getCurrentTimeZone
     let time = utcToLocalTime zone ex
     ck <- set name "sid" . set value (show sd) <$> hNewCookie time True
     hSetCookie (fromList [ck])

-- | Given the (possibly wrong) request cookie, try to recover the existing
-- session identifier.

getCookieSessionID :: (MonadIO m, HttpM Request m) => m (Maybe SessionID)
getCookieSessionID =
  -- todo : join . fmap == (=<<)
  fmap SID . join . fmap readMay . join . fmap (get (fmapL value . pickCookie "sid"))
     <$> hCookie

delCookieSession :: HttpM Response m => m ()
delCookieSession = hDelCookie "sid"

-- | Create a new session with a specified expiration date. The session will be
-- stored in the session map.

newSessionVar :: (MonadIO m, PayloadM q (Sessions p) m) => m (TVar (Session p))
newSessionVar = do
  t <- liftIO getCurrentTime
  session <- newVar (Session undefined t t 0 Nothing)
  keys <- liftIO newStdGen >>= return . randoms
  sd <- payload $ do
    sd <- newSessionID keys
    S.modify . withSessions $ M.insert sd session
    return sd
  modVar (set sID sd) session

newSessionID :: (MonadState (Sessions p) m, Functor m) => [SessionID] -> m SessionID
newSessionID keys =
  do store <- unSessions <$> S.get
     return . head . filter (flip M.notMember store) $ keys

willExpireAt :: Session p -> UTCTime
willExpireAt session = fromInteger (get sExpire session) `addUTCTime` get sLast session 

lookupSessionVar
  :: (MonadIO m, PayloadM q (Sessions p) m)
  => SessionID -> m (Maybe (TVar (Session p)))
lookupSessionVar sd = payload (S.gets (M.lookup sd . unSessions))

-- STM utilities.

newVar :: MonadIO m => a -> m (TVar a)
getVar :: MonadIO m => TVar a -> m a
putVar :: MonadIO m => TVar a -> a -> m ()
modVar :: MonadIO m => (a -> a) -> TVar a -> m (TVar a)

newVar       = liftIO . atomically . newTVar
getVar       = liftIO . atomically . readTVar
putVar   var = liftIO . atomically . writeTVar var
modVar f var = liftIO . atomically $ (readTVar var >>= writeTVar var . f) >> return var

