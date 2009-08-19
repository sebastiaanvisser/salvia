module Network.Salvia.Handlers

  -- * Fundamental protocol handlers.

  -- ** Default handler environments.

  ( hDefaultEnv
  , hSessionEnv

  -- ** Parse client requests.

  , hRequestParser
  , hResponseParser
  , hParser
  , readNonEmptyLines

  -- ** Print server responses.
  
  , hResponsePrinter
  , hRequestPrinter
  , hFlushHeaders
  , hFlushQueue

  -- ** HTTP header banner.

  , hBanner

  -- ** Closing or keeping alive connections.

  , hCloseConn
  , hKeepAlive

  -- ** Enable HTTP HEAD requests.
  
  , hHead

  -- * Error handling and logging.

  -- ** Default error handlers.

  , hError
  , hCustomError
  , hIOError
  , hSafeIO

  -- ** Logging of client requests.

  , hLog
  , hLogWithCounter

  -- ** Request counter.

  , hCounter

-- * Redirecting and rewriting.

  -- ** Redirecting the client.

  , hRedirect

  -- ** Request URI rewriting.

  , hRewrite
  , hRewriteHost
  , hRewritePath
  , hRewriteExt
  , hWithDir
  , hWithoutDir

  -- * File and directory serving.

  -- ** Serve static file resources.

  , hFileResource
  , hFileResourceFilter
  , hResource
  , fileMime
  , hUri
  , hFile
  , hFileFilter

  -- ** Serve directory indices.

  , hDirectory
  , hDirectoryResource

  -- ** Serve file system directory.

  , hFileTypeDispatcher
  , hFileSystem
  , hFileSystemNoIndexes

  -- ** Enable PUTing resources to the files ystem.

  , hPut

  -- ** Serving CGI scripts.

  , hCGI

  -- * Dispatching.

  -- ** Custom request dispatchers.

  , Dispatcher
  , ListDispatcher
  , hDispatch
  , hRequestDispatch
  , hListDispatch

  -- ** Dispatch based on request method.

  , hMethodRouter

  -- ** Dispatch based on request path.

  , hPath
  , hPathRouter
  , hPrefix
  , hPrefixRouter
  , hParameters

  -- ** Dispatch based on filename extension.

  , hExtension
  , hExtensionRouter

  -- ** Dispatch based on host name.

  , hVirtualHosting
  , hPortRouter

  -- * Session and user management.

  -- ** Cookie handling.

  , hSetCookies
  , hGetCookies
  , newCookie

  -- ** Session management.

  , hSession

  , SessionID
  , Session (..)
  , TSession
  , Sessions

  , hSessionID
  , hSetSessionCookie

  , mkSessions

  -- ** User management.

  , Username
  , Password
  , Action
  , Actions
  , User (..)
  , Users
  , UserDatabase
  , TUserDatabase

  , UserPayload (..)
  , UserSession
  , TUserSession

  , hSignup
  , hLogin
  , hLogout
  , hLoginfo

  , hAuthorized
  , hAuthorizedUser

  , readUserDatabase
  )
where

import Network.Salvia.Handler.Banner
import Network.Salvia.Handler.CGI
import Network.Salvia.Handler.Close
import Network.Salvia.Handler.Cookie
import Network.Salvia.Handler.Counter
import Network.Salvia.Handler.Directory
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Handler.Environment
import Network.Salvia.Handler.Error
import Network.Salvia.Handler.ExtensionDispatcher
import Network.Salvia.Handler.File
import Network.Salvia.Handler.FileSystem
import Network.Salvia.Handler.Head
import Network.Salvia.Handler.Log
import Network.Salvia.Handler.Login
import Network.Salvia.Handler.MethodRouter
import Network.Salvia.Handler.Parser
import Network.Salvia.Handler.PathRouter
import Network.Salvia.Handler.Printer
import Network.Salvia.Handler.Put
import Network.Salvia.Handler.Redirect
import Network.Salvia.Handler.Rewrite
import Network.Salvia.Handler.Session
import Network.Salvia.Handler.VirtualHosting

