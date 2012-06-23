{-# LANGUAGE OverloadedStrings #-}

module Proxy
       ( HostMap
       , RouteMap
       , Address (..)
       , backendQueueDaemon
       , createBackendQueue
       , createBackendRequest
       , createProxy
       , createResponse
       , prefixMatch
       , usedPorts
       ) where

import Prelude
import Data.List (sortBy)
import Yesod (liftIO)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.HTTP.Types as H

import Data.ByteString.UTF8 (ByteString)

import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map as Map
import GHC.Conc (atomically, STM, TVar, newTVar, readTVar, writeTVar)

-- conduit imports
--import qualified Data.Conduit as C
--import Data.Conduit.Binary (sinkFile)
import qualified Network.HTTP.Conduit as HC
--import Control.Monad.Trans.Resource (ResourceT)

--
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM.TBMChan
import Control.Monad (forever)


data Backend = Backend { backendHost :: ByteString
                       , backendPort :: Int
                       , backendLimit :: TVar Int
                       , backendCounter :: TVar Int
                       }

type BackendQueue = TBMChan (MVar Backend)

data Address  = Address { host :: ByteString
                        , port :: Int
                        , queue :: BackendQueue
                        }

type RouteMap = Map.Map ByteString Address
type HostMap  = Map.Map ByteString RouteMap
type BackendMap = RouteMap
type SiteMap = HostMap


modTVar :: TVar Int -> (Int -> Int) -> STM Int
modTVar tvar f = do
               c <- readTVar tvar
               let newValue = f c
               writeTVar tvar newValue
               return newValue


createBackend :: ByteString -> Int -> Int -> STM Backend
createBackend host' port' limit' = do
              limitTVar <- newTVar limit'
              counterTVar <- newTVar 0
              return $! Backend host' port' limitTVar counterTVar


createBackendQueue :: Int -> STM (BackendQueue)
createBackendQueue = newTBMChan


backendQueueDaemon :: BackendQueue -> IO ()
backendQueueDaemon queue' = forever $ do
                   freeSlots <- atomically $ estimateFreeSlotsTBMChan queue'
                   case freeSlots > 0 of
                        True -> do
                             mmvar <- atomically $ readTBMChan queue'
                             case mmvar of
                                  Just mvar -> do
                                       backend' <- atomically $ createBackend "" 0 0 -- FIXME: fill in proper backend
                                       _ <- liftIO $ putMVar mvar $ backend'
                                       return ()
                                  Nothing -> return () --FIXME: Channel was closed, shut down.
                             return ()
                        False -> return ()


createProxy :: TVar HostMap -> Int -> IO ()
createProxy routeMapTVar port' = do
            manager <- HC.newManager HC.def

            run port' $ application manager routeMapTVar
            HC.closeManager manager


usedPorts :: HostMap -> [Int]
usedPorts hostMap =  map port $ concat $ map Map.elems $ Map.elems hostMap


prefixMatch :: Ord a => [a] -> [a] -> (Int)
prefixMatch (a:as) (b:bs) =
            if a == b
            then 1 + (prefixMatch as bs)
            else 0

prefixMatch _ _ = 0

prefixMatch' :: ByteString -> (ByteString, Address) -> (Int, (ByteString, Address))
prefixMatch' needle t@(route', _) = (prefixMatch (unpack needle) (unpack route'), t)


findBackend :: TVar HostMap -> ByteString
            -> ByteString -> IO (Maybe (Address, Int))
findBackend hostMapTVar host' route = do
            hostMap <- liftIO $ atomically $ readTVar hostMapTVar

            return $! case Map.lookup host' hostMap of
                 Just routeMap ->
                      case Map.toList routeMap of
                           [] -> Nothing
                           li -> let a = map     (prefixMatch' route)               li
                                     b = filter  (\(i',_)        -> i' > 1)         a
                                     c = sortBy  (\(a',_) (b',_) -> compare a' b')  b
                                 in case c of
                                    ((n, (_, m)):_) -> Just (m, n)
                                    _ -> Nothing
                 Nothing       -> Nothing


contentType ct = ("Content-Type", ct)


createBackendRequest :: Request -> (Address, Int) -> HC.Request m
createBackendRequest req (address, matchLength) =
                     let (_, rest) = splitAt matchLength (unpack $ rawPathInfo req)
                         url = pack $ rest
                     in
                         HC.def { HC.method = requestMethod req
                                , HC.host = host address
                                , HC.port = port address
                                , HC.path = url
                                , HC.queryString = rawQueryString req
                                , HC.requestHeaders = requestHeaders req
                                  ++ [("X-Forwarded-For", "localhost:5000")]
--                                , HC.requestBody = HC.RequestBodyLBS requestBodyString
                                , HC.redirectCount = 0
                                , HC.checkStatus = (\_ _ -> Nothing)
                                }


createResponse :: H.Status -> H.ResponseHeaders -> LB.ByteString -> Response
createResponse status headers src =
             case H.statusCode status of
                    200 -> responseLBS status headers src
                    _   -> responseLBS status headers src


application :: HC.Manager -> TVar HostMap -> Application
application manager hostMapTVar req = do
            backend <- liftIO $ findBackend hostMapTVar
                                            (serverName req)
                                            (rawPathInfo req)

            case backend of
                 Nothing -> return $! responseLBS H.status404
                                                  [contentType "text/plain"]
                                                  ":-("
                 Just addressAndMatchLength@(address, _) -> do
                      let backendRequest = createBackendRequest req addressAndMatchLength

                      mvar <- liftIO $ newEmptyMVar
                      addToQueueResult <- liftIO $ atomically $ tryWriteTBMChan (queue address) mvar

                      case addToQueueResult of
                           Just True  -> do
                                _ <- liftIO $ takeMVar mvar
                                HC.Response status _ headers src <-
                                            HC.httpLbs backendRequest manager

                                return $! createResponse status headers src

                           Just False -> return $! responseLBS H.status500
                                                               [contentType "text/plain"]
                                                               "Application overloaded. Try again later."

                           Nothing    -> return $! responseLBS H.status500
                                                               [contentType "text/plan"]
                                                               "Application has been shut down."
