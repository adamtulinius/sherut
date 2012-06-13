{-# LANGUAGE OverloadedStrings #-}

module Proxy
       (createProxy
       ) where

import Prelude
import Yesod (liftIO)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.HTTP.Types as H
--import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString (append)
import Data.ByteString.UTF8 (ByteString)
--import Data.Enumerator (run_, enumList, ($$))
--import qualified Data.Text as T
import Data.ByteString.Char8 (pack, unpack)
--import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map as Map
import GHC.Conc (atomically, STM, TVar, readTVar, writeTVar)

-- conduit imports
--import qualified Data.Conduit as C
--import Data.Conduit.Binary (sinkFile)
import qualified Network.HTTP.Conduit as HC
--import Control.Monad.Trans.Resource (ResourceT)


createProxy :: TVar (Map.Map ByteString (ByteString, Int)) -> Int -> IO ()
createProxy routeMapTVar port = do

     manager <- HC.newManager HC.def

     run port $ application manager routeMapTVar
     HC.closeManager manager


findBackend :: TVar (Map.Map ByteString (ByteString, Int)) -> ByteString
            -> ByteString -> IO (Maybe (ByteString, Int))
findBackend routeMapTVar host route = do
            routeMap <- liftIO $ atomically $ readTVar routeMapTVar

            return $! Map.lookup (host `append` route) routeMap


contentType ct = ("Content-Type", ct)


application :: HC.Manager -> TVar (Map.Map ByteString (ByteString, Int)) -> Application
application manager routeMapTVar req = do
            backend <- liftIO $ findBackend routeMapTVar (serverName req)
                                                (rawPathInfo req)
            case backend of
                 Nothing -> return $! responseLBS H.status404 [contentType "text/html"] ":-("
                 Just (backendHost, backendPort) -> do
--                      let (_, _, requestBodyString) = requestBody req
                      let backendRequest = HC.def { HC.method = requestMethod req
                                                  , HC.host = backendHost
                                                  , HC.port = backendPort
                                                  , HC.path = rawPathInfo req
                                                  , HC.queryString = rawQueryString req
                                                  , HC.requestHeaders = requestHeaders req
                                                          ++ [("X-Forwarded-For", "localhost:5000")]
--                                                  , HC.requestBody = HC.RequestBodyLBS requestBodyString
                                                  , HC.redirectCount = 0
                                                  , HC.checkStatus = (\_ _ -> Nothing)
                                                  }


                      HC.Response status _ headers src <-
                                  HC.httpLbs backendRequest manager

                      return $! case H.statusCode status of
                           200 -> responseLBS status headers src
                           302 -> responseLBS status headers src
                           _   -> responseLBS status headers ""
