{-# LANGUAGE OverloadedStrings #-}

module Proxy
       ( HostMap
       , RouteMap
       , Address (..)
       , createProxy
       , prefixMatch
       , usedPorts
       ) where

import Prelude
import Data.List (sortBy, splitAt)
import Yesod (liftIO)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.HTTP.Types as H
--import Blaze.ByteString.Builder (copyByteString)
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


data Address  = Address { host :: ByteString, port :: Int } deriving (Show)
type RouteMap = Map.Map ByteString Address
type HostMap  = Map.Map ByteString RouteMap


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


application :: HC.Manager -> TVar HostMap -> Application
application manager hostMapTVar req = do
            backend <- liftIO $ findBackend hostMapTVar
                                            (serverName req)
                                            (rawPathInfo req)

            case backend of
                 Nothing -> return $! responseLBS H.status404 [contentType "text/html"] ":-("
                 Just (address, matchLength) -> do
--                      let (_, _, requestBodyString) = requestBody req
                      let (match, rest) = splitAt matchLength (unpack $ rawPathInfo req)
                          backendRequest = HC.def { HC.method = requestMethod req
                                                  , HC.host = host address
                                                  , HC.port = port address
                                                  , HC.path = pack $ rest
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
                             _   -> responseLBS status headers src
