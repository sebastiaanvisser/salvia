module Network.Salvia.Handler.Log where

import Control.Monad.State
import Data.List
import Data.Record.Label
import Network.Protocol.Http
import Network.Salvia.Interface
import System.IO

{- |
A simple logger that prints a summery of the request information to the
specified file handle.
-}

hLog :: (AddressM' m , MonadIO m, HttpM' m) => Handle -> m ()
hLog handle =
  do mt <- request  (getM method)
     ur <- request  (getM uri)
     st <- response (getM status)
     dt <- response (getM date)
     ca <- clientAddress
     sa <- serverAddress
     let code = codeFromStatus st
     liftIO
       . hPutStrLn handle
       $ intercalate " ; "
         [ maybe "" id dt
         , show sa
         , show mt
         , show ca
         , ur
         , show code ++ " " ++ show st
         ]

