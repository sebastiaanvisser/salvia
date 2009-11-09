{-# LANGUAGE TemplateHaskell #-}
module Network.Salvia.Handler.Login
(
-- * Basic types.

  Username
, Password
, Action
, User (User)
, username
, password
, actions

-- * Login server aspect.

, LoginM (..)

-- * User Sessions.

, UserPayload (..)
, UserSession

-- * User database backend.

, UserDatabase (UserDatabase)
, users
, backend

, Backend (..)
, noBackend
, fileBackend

-- * Handlers.

, hGetUser
, hSignup
, hLogin
, hLogout
, hLoginfo
, hAuthorized

)
where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.State hiding (get)
import Data.ByteString.Lazy.UTF8 hiding (lines)
import Data.Digest.Pure.MD5
import Data.List
import Data.Maybe
import Data.Record.Label
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Core.Aspects
import Network.Salvia.Handler.Body
import Network.Salvia.Handler.Session
import Prelude hiding (mod)
import Safe

{- |
User containg a username, password and a list of actions this user is allowed
to perform within the system.
-}

type Username = String
type Password = String
type Action   = String

data User = User
  { _username :: Username
  , _password :: Password
  , _actions  :: [Action]
  } deriving (Eq, Show)

$(mkLabels [''User])

username :: User :-> Username
password :: User :-> Password
actions  :: User :-> [Action]

{- |
A user payload instance contains user related session information and can be
used as the payload for regular sessions. It contains a reference to the user
it belongs to, a flag to indicate whether the user is logged in or not and a
possible user specific session payload.
-}

data UserPayload a = UserPayload
  { upUser     :: User
  , upLoggedIn :: Bool
  , upPayload  :: Maybe a
  } deriving (Eq, Show)

type UserSession a = Session (UserPayload a)

data Backend = Backend
  { read :: MonadIO m => m UserDatabase
  , add  :: MonadIO m => User -> m ()
  }

{- |
A user database containing a list of users and a reference to the backend the
database originates from and can be synchronized back to.
-}

data UserDatabase = UserDatabase
  { _backend :: Backend
  , _users   :: [User]
  }

$(mkLabels [''UserDatabase])

users   :: UserDatabase :-> [User]
backend :: UserDatabase :-> Backend

class (Applicative m, Monad m) => LoginM m p | m -> p where
  login      :: TVar UserDatabase ->             m a -> (User -> m a) -> m a
  logout     ::                                                          m ()
  signup     :: TVar UserDatabase -> [Action] -> m a -> (User -> m a) -> m a
  authorized :: Maybe Action      ->             m a -> (User -> m a) -> m a

hGetUser :: LoginM m p => m (Maybe User)
hGetUser = authorized Nothing (return Nothing) (return . Just)

{- |
The signup handler is used to create a new entry in the user database. It reads
a new username and password from the post parameters and adds a new entry into
the backend of the user database when no user with such name exists. The user
gets the specified initial set of actions assigned. When the signup fails the
first handler will be executed when the signup succeeds the second handler will
be executed which may access the fresh user object.
-}

hSignup
  :: (MonadIO m, SessionM m (UserPayload p), BodyM Request m, HttpM Request m)
  => p -> TVar UserDatabase -> [Action] -> m a -> (User -> m a) -> m a
hSignup _ tdb acts onFail onOk =
  do ps <- hRequestParameters "utf-8"
     join . liftIO . atomically $
       do db <- readTVar tdb
          case freshUserInfo ps (get users db) acts of
            Nothing -> return onFail
            Just user -> 
              do writeTVar tdb (mod users (user:) db)
                 return $
                   do add (get backend db) user
                      onOk user

-- | Helper functions that generates fresh user information.

freshUserInfo :: Maybe Parameters -> [User] -> [Action] -> Maybe User
freshUserInfo ps us acts =
  do p <- ps
     user <- "username" `lookup` p >>= id
     pass <- "password" `lookup` p >>= id
     if null user || null pass
       then Nothing
       else case headMay $ filter ((==user) . get username) us of
              Nothing -> return $ User user (show (md5 (fromString pass))) acts
              Just _  -> Nothing

{- |
The login handler. Read the username and password values from the post data and
use that to authenticate the user. When the user can be found in the database
the user is logged in and stored in the session payload. When the login fails
the first handler will be executed when the login succeeds the second handler
will be executed which may access the fresh user object.
-}

hLogin
  :: (SessionM m (UserPayload p), HttpM Request m, MonadIO m, BodyM Request m)
  => p -> TVar UserDatabase -> m a -> (User -> m a) -> m a
hLogin _ tdb onFail onOk =
  do ps <- hRequestParameters "utf-8"
     db <- (liftIO . atomically . readTVar) tdb
     case authenticate ps db of
       Nothing   -> onFail
       Just user -> do let pl = Just (UserPayload user True Nothing)
                       withSession (set sPayload pl)
                       onOk user

-- | Helper functions that performs the authentication check.

authenticate :: Maybe Parameters -> UserDatabase -> Maybe User
authenticate ps db =
  do p <- ps
     user <- "username" `lookup` p >>= id
     pass <- "password" `lookup` p >>= id
     case headMay $ filter ((==user) . get username) (get users db) of
       Just u | get password u == show (md5 (fromString pass)) -> Just u
       _                                                       -> Nothing

-- | Logout the current user by emptying the session payload.

hLogout :: SessionM m (UserPayload p) => p -> m ()
hLogout _ = withSession (set sPayload Nothing)

{- |
The `loginfo' handler exposes the current user session to the world using a
simple text based response. The response contains information about the current
session identifier, session start and expiration date and the possible user
payload that is included.
-}

hLoginfo :: (SessionM m (UserPayload p), SendM m) => m ()
hLoginfo =
  do hSessionInfo
     s <- getSession
     case get sPayload s of
       Nothing -> return ()
       Just (UserPayload (User uname _ acts) _ _) ->
         do send $ "\n" ++ intercalate "\n"
              [ "username=" ++ uname
              , "actions="  ++ intercalate " " acts
              ]

{- |
Execute a handler only when the user for the current session is authorized to
do so. The user must have the specified action contained in its actions list in
order to be authorized. When the authorization fails the first handler will be
executed when the authorization succeeds the second handler will be executed
which may access the current user object. 
-}

hAuthorized :: SessionM m (UserPayload p) => p -> Maybe Action -> m b -> (User -> m b) -> m b
hAuthorized _ maction onFail onOk =
  do session <- getSession
     case (maction, get sPayload session) of
       (Nothing,     Just (UserPayload user _ _))                                  -> onOk user
       (Just action, Just (UserPayload user _ _)) | action `elem` get actions user -> onOk user
       _                                                                           -> onFail

-- | User database backend that does nothing and discards all changes made.

noBackend :: Backend
noBackend = let b = Backend (return (UserDatabase b [])) (const (return ())) in b

-- | File based user database backend. Format: /username password action*/.

fileBackend :: FilePath -> Backend
fileBackend file = bcknd
  where 
    bcknd = Backend
      ((UserDatabase bcknd . parse) `liftM` liftIO (readFile file))
      (liftIO . appendFile file . printUserLine)
    parse = catMaybes . map parseUserLine . lines
    parseUserLine line =
      case (line, words line) of
        ('#':_,          _) -> Nothing
        (_, user:pass:acts) -> Just (User user pass acts)
        _                   -> Nothing
    printUserLine u = intercalate " " $
      [ get username u
      , get password u
      ] ++ get actions u ++ ["\n"]

