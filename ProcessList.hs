{-# LANGUAGE OverloadedStrings #-}

module ProcessList
    ( createChildApp
    , killChildApp
    , module GHC.Conc
    , module System.Process
    ) where

import Prelude
import Data.List (sort)
--import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.Process
import GHC.Conc (atomically, STM, TVar, newTVar, readTVar, writeTVar)
import qualified Data.Map as Map
import Data.ByteString.UTF8 (ByteString)
import Control.Concurrent (forkIO)
import Proxy
import Misc


createChildApp :: TVar HostMap -> TVar [TVar ChildApp]
               -> ByteString -> ByteString -> String -> String -> FilePath
               -> [Maybe String] -> IO (TVar ChildApp)


createChildApp hostMapTVar storage host' route' name' version' filePath' args' = do
               (child, queue') <- atomically $ do
                          hostMap <- readTVar hostMapTVar

                          let sortedPorts = sort $ usedPorts hostMap
                              np (x1:[])                = x1+1
                              np (x1:x2:xs) | diff <= 1 = np(x2:xs)
                                            | diff  > 1 = x1+1
                                 where diff = x2-x1
                              np _                      = 3001
                              newPort = np sortedPorts

                              mHostRoutes = Map.lookup host' hostMap

                          newChild <- newTVar (ChildApp name' version' filePath' args'
                                                        newPort Nothing Started)
                          --FIXME: do something sensible with max queue length
                          backendQueue <- createBackendQueue 100
                          let address = Address host' newPort backendQueue

                              newHostRoutes = case mHostRoutes of
                                 Nothing         -> Map.insert route' address Map.empty
                                 Just hostRoutes -> Map.insert route' address hostRoutes

                          _ <- writeTVar hostMapTVar $ Map.insert host' newHostRoutes hostMap

                          _ <- addChildApp storage newChild

                          return (newChild, backendQueue)

               _ <- forkIO $ backendQueueDaemon queue'
               _ <- spawnChildApp storage child
               return child
