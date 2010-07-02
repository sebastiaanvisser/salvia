module Network.Salvia.Handlers {- todo doc - client/server assumptions -}
(

-- * Fundamental protocol handlers.

-- ** Default handler environments.

  hDefaultEnv
, hEnvNoKeepAlive

-- ** Parse client requests.

, hRequestParser
, hResponseParser
, hParser
, readNonEmptyLines

-- ** Print server responses.

, hResponsePrinter
, hRequestPrinter
, hFlushHeaders
, hFlushHeadersOnly
, hFlushRequestHeaders
, hFlushResponseHeaders
, hFlushQueue

-- ** Accessing request and response bodies.

-- *** Access the message body as raw ByteString.

, hRawBody
, hRawRequestBody
, hRawResponseBody

-- *** Access the message body as Text.

, hBodyText
, hRequestBodyText
, hResponseBodyText

-- *** Access the message body as UTF8 String.

, hBodyStringUTF8
, hRequestBodyStringUTF8
, hResponseBodyStringUTF8

-- *** Access the message body as URI encoded parameters.

, hParameters
, hRequestParameters
, hResponseParameters

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
, hDumpRequest
, hDumpResponse

-- * Redirecting and rewriting.

-- ** Redirecting the client.

, hLocalRequest
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

-- ** Support for HTTP ranges.

, Range (..)
, contentRange
, range
, rangeL

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

, hMethod
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

-- * Cookie management.

, hSetCookie
, hCookie
, hDelCookie
, hNewCookie
)
where

-- todo: cleanup handler exports and export entire modules?

import Network.Salvia.Handler.Banner
import Network.Salvia.Handler.Body
import Network.Salvia.Handler.CGI
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
import Network.Salvia.Handler.Method
import Network.Salvia.Handler.Parser
import Network.Salvia.Handler.Path
import Network.Salvia.Handler.Printer
import Network.Salvia.Handler.Put
import Network.Salvia.Handler.Range
import Network.Salvia.Handler.Redirect
import Network.Salvia.Handler.Rewrite
import Network.Salvia.Handler.VirtualHosting

