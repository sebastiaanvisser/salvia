module Network.Salvia.Handler.Login (

  -- * Basic types.
    Username
  , Password
  , Action
  , Actions
  , User (..)
  , Users
  , UserDatabase
  , TUserDatabase

  -- * User Sessions.
  , UserPayload (..)
  , UserSession
  , TUserSession

  -- * Handlers.
  , hSignup
  , hLogin
  , hLogout
  , hLoginfo

  , hAuthorized
  , hAuthorizedUser

  -- * Helper functions.
  , readUserDatabase

  ) where

import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, newTVar)
import Control.Monad.State
import Data.Digest.Pure.MD5 (md5)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Record.Label
import Misc.Misc (atomModTVar, safeHead)
import Network.Protocol.Http (Status (Unauthorized, OK), status)
import Network.Salvia.Handler.Error (hCustomError, hError)
import Network.Salvia.Handler.Session
import Network.Salvia.Httpd hiding (email)
import Network.Protocol.Uri
import Data.ByteString.Lazy.UTF8 (fromString)

-------------------------------------------------------------------------------

type Username = String
type Password = String
type Action   = String
type Actions  = [Action]

{- |
User containg a username, password and a list of actions this user is allowed
to perform within the system.
-}

data User = User {
    username :: Username
  , password :: Password
  , email    :: String
  , actions  :: Actions
  } deriving (Eq, Show)

type Users = [User]

{- |
A user database containing a list of users, a list of default actions the guest
or `no-user' user is allowed to perform and a polymorphic reference to the
place the database originates from. This source field can be used by update
functions synchronizing changes back to the database.
-}

data UserDatabase src =
  UserDatabase {
    dbUsers  :: Users
  , dbGuest  :: Actions
  , dbSource :: src
  } deriving Show

type TUserDatabase src = TVar (UserDatabase src)

{- |
A user payload instance contains user related session information and can be
used as the payload for regular sessions. It contains a reference to the user
it is bound to, a flag to indicate whether the user is logged in or not and a
possible user specific session payload.
-}

data UserPayload a = UserPayload {
    upUser     :: User
  , upLoggedIn :: Bool
  , upPayload  :: Maybe a
  } deriving (Eq, Show)

type UserSession  a = Session  (UserPayload a)
type TUserSession a = TSession (UserPayload a)

{- | A handler that requires a session with a user specific payload. -}

-- type UserSessionHandler a b = SessionHandler (UserPayload a) b

-------------------------------------------------------------------------------

{-|
Read a user data from file. Format: /username password email action*/.
-}

readUserDatabase :: FilePath -> IO (TUserDatabase FilePath)
readUserDatabase file = do

  -- First line contains the default `guest` actions, tail lines contain users.
  gst:ls <- lines `liftM` readFile file

  atomically $ newTVar $ UserDatabase
    (catMaybes $ map parseUserLine ls)
    (words gst)
    file
  where
    parseUserLine line =
      case words line of
        user:pass:mail:acts -> Just (User user pass mail acts)
        _                   -> Nothing

printUserLine :: User -> String
printUserLine u = intercalate " " ([
    username u
  , password u
  , email u
  ] ++ actions u)

{- |
The signup handler is used to create a new entry in the user database. It reads
a new username and password from the HTTP POST parameters and adds a new entry
in the database when no user with such name exists. The user gets the specified
initial set of actions assigned. On failure an `Unauthorized' error will be
produced.
-}

hSignup :: (MonadIO m, Receive m, Response m, Send m) => TUserDatabase FilePath -> Actions -> m ()
hSignup tdb acts = do
  db <- liftIO . atomically $ readTVar tdb
  params <- uriEncodedPostParamsUTF8
  case freshUserInfo params (dbUsers db) acts of
    Nothing -> hCustomError Unauthorized "signup failed"
    Just u  -> do
      liftIO $ do
        atomically
          $ writeTVar tdb
          $ UserDatabase (u : dbUsers db) (dbGuest db) (dbSource db)
        appendFile (dbSource db) (printUserLine u)

freshUserInfo :: Maybe Parameters -> Users -> Actions -> Maybe User
freshUserInfo params us acts = do
  p <- params
  user <- "username" `lookup` p >>= id
  pass <- "password" `lookup` p >>= id
  mail <- "email"    `lookup` p >>= id
  case safeHead $ filter ((==user).username) us of
    Nothing -> return $ User user (show $ md5 $ fromString pass) mail acts
    Just _  -> Nothing

-------------------------------------------------------------------------------

{- |
The login handler. Read the username and password values from the post data and
use that to authenticate the user. When the user can be found in the database
the user is logged in and stored in the session payload. Otherwise a
`Unauthorized' response will be sent and the user has not logged in.
-}

hLogin :: (MonadIO m, Receive m, Response m, Send m) => UserDatabase b -> TUserSession a -> m ()
hLogin db session = do
  params <- uriEncodedPostParamsUTF8
  maybe
    (hCustomError Unauthorized "login failed")
    (loginSuccessful session)
    (authenticate params db)

authenticate :: Maybe Parameters -> UserDatabase a -> Maybe User
authenticate params db = do
  p <- params
  user <- "username" `lookup` p >>= id
  pass <- "password" `lookup` p >>= id
  case safeHead $ filter ((==user).username) (dbUsers db) of
    Nothing -> Nothing
    Just u  ->
      if password u == (show $ md5 $ fromString pass)
      then return u
      else Nothing

-- Login user and create `Ok' response on successful user.
loginSuccessful :: (MonadIO m, Response m, Send m) => TUserSession a -> User -> m ()
loginSuccessful session user = do
  liftIO $ atomModTVar (\s -> s {sPayload = Just (UserPayload user True Nothing)}) session
  response (setM status OK)
  sendStrLn "login successful"

-------------------------------------------------------------------------------

{- | Logout the current user by emptying the session payload. -}

hLogout :: MonadIO m => TUserSession a -> m ()
hLogout session = do
  liftIO $ atomModTVar (\s -> s {sPayload = Nothing}) session
  return ()

-------------------------------------------------------------------------------

{- |
The `loginfo' handler exposes the current user session to the world using a
simple text based file. The file contains information about the current session
identifier, session start and expiration date and the possible user payload
that is included.
-}

hLoginfo :: (MonadIO m, Send m) => TUserSession a -> m ()
hLoginfo session = do
  s' <- liftIO $ atomically $ readTVar session

  sendStrLn $ "sID="    ++ show (sID     s')
  sendStrLn $ "start="  ++ show (sStart  s')
  sendStrLn $ "expire=" ++ show (sExpire s')

  case sPayload s' of
    Nothing -> return ()
    Just (UserPayload (User uname _ mail acts) _ _) -> do
      sendStrLn $ "username=" ++ uname
      sendStrLn $ "email="    ++ mail
      sendStrLn $ "actions="  ++ intercalate " " acts

-------------------------------------------------------------------------------

{- |
Execute a handler only when the user for the current session is authorized to
do so. The user must have the specified action contained in its actions list in
order to be authorized. Otherwise an `Unauthorized' error will be produced.
When no user can be found in the current session or this user is not logged in
the guest account from the user database is used for authorization.
-}

hAuthorized
  :: (MonadIO m, Response m, Send m)
  => UserDatabase b        -- ^ The user database to read guest account from.
  -> Action                -- ^ The actions that should be authorized.
  -> (Maybe User -> m ())  -- ^ The handler to perform when authorized.
  -> TUserSession a        -- ^ This handler requires a user session.
  -> m ()  

hAuthorized db action handler session = do
  load <- liftM sPayload (liftIO $ atomically $ readTVar session)
  case load of
    Just (UserPayload user _ _)
      | action `elem` actions user -> handler (Just user)
    Nothing
      | action `elem` dbGuest db   -> handler Nothing
    _                              -> hError Unauthorized

{- |
Execute a handler only when the user for the current session is authorized to
do so. The user must have the specified action contained in its actions list in
order to be authorized. Otherwise an `Unauthorized' error will be produced. The
guest user will not be used in any case.
-}

hAuthorizedUser
  :: (MonadIO m, Response m, Send m)
  => Action          -- ^ The actions that should be authorized.
  -> (User -> m ())  -- ^ The handler to perform when authorized.
  -> TUserSession a  -- ^ This handler requires a user session
  -> m ()    

hAuthorizedUser action handler session = do
  load <- liftM sPayload (liftIO $ atomically $ readTVar session)
  case load of
    Just (UserPayload user _ _)
      | action `elem` actions user -> handler user
    _                              -> hError Unauthorized

