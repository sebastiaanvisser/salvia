module Network.Salvia.Handlers {- todo doc - client/server assumptions -}
(

-- * Fundamental protocol handlers.

-- ** Default handler environments.

  hDefaultEnv

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

-- ** Accessing request and response bodies.

, hRawRequestBody
, hRawResponseBody
, hRawBody

, hRequestBody
, hResponseBody
, hBody

, hRequestParameters
, hResponseParameters
, hParameters

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

, hPutFileSystem
, hPutResource
, hStore

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
, hQueryParameters

-- ** Dispatch based on filename extension.

, hExtension
, hExtensionRouter

-- ** Dispatch based on host name.

, hVirtualHosting
, hPortRouter

-- * HTTP client and proxy.

-- ** Client requests.

, hGetRequest
, hClientEnvironment

-- ** Proxy.

--, hProxy

-- * Session management.

-- ** Cookie handling.

, hSetCookie
, hCookie
, hDelCookie
, hNewCookie

-- ** Session identifier.

, SessionID (SID)
, sid

-- ** Session data type.

, Session (Session)
, sID
, sStart
, sLast
, sExpire
, sPayload 

-- ** Session interface through type class.

, SessionM (..)

-- ** Collection of sessions.

, Sessions
, mkSessions 

-- ** Session handlers.

, hProlongSession
, hGetSession
, hPutSession
, hDelSession
, hWithSession
, hSessionInfo

-- * User management.

-- ** Basic types.

, Username
, Password
, Action
, User (User)
, username
, password
, actions

-- ** Login server aspect.

, LoginM (..)

-- ** User Sessions.

, UserPayload (..)
, UserSession

-- ** User database backend.

, UserDatabase (UserDatabase)
, users
, backend

, Backend (..)
, noBackend
, fileBackend

-- ** Handlers.

, hSignup
, hLogin
, hLogout
, hLoginfo
, hAuthorized

)
where

-- todo: cleanup handler exports and export entire modules?

-- import Network.Salvia.Handler.Proxy
import Network.Salvia.Handler.Banner
import Network.Salvia.Handler.Body
import Network.Salvia.Handler.CGI
import Network.Salvia.Handler.Client
import Network.Salvia.Handler.Close
import Network.Salvia.Handler.Cookie
import Network.Salvia.Handler.Directory
import Network.Salvia.Handler.Dispatching
import Network.Salvia.Handler.Environment
import Network.Salvia.Handler.Error
import Network.Salvia.Handler.Extension
import Network.Salvia.Handler.File
import Network.Salvia.Handler.FileSystem
import Network.Salvia.Handler.Head
import Network.Salvia.Handler.Log
import Network.Salvia.Handler.Login
import Network.Salvia.Handler.Method
import Network.Salvia.Handler.Parser
import Network.Salvia.Handler.Path
import Network.Salvia.Handler.Printer
import Network.Salvia.Handler.Put
import Network.Salvia.Handler.Redirect
import Network.Salvia.Handler.Rewrite
import Network.Salvia.Handler.Session
import Network.Salvia.Handler.VirtualHosting

