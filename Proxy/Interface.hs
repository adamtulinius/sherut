{-# LANGUAGE OverloadedStrings #-}

module Proxy.Interface
       ( newBackend
       , newBackendChannel
       ) where

import Prelude
import GHC.Conc (atomically, STM, TVar, readTVar, writeTVar)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Monad (filterM, forever)
import Control.Monad.STM
import Proxy
import Misc

-- http
import Network.Wai (Request)


-- spawnChildApp :: TVar [TVar ChildApp] -> TVar ChildApp -> IO ()

newBackendChannel :: STM (TChan Request)
newBackendChannel = newTChan

-- simReq = number of simultanious requests to the backend
newBackend :: TChan Request -> Int -> TVar [TVar ChildApp] -> TVar ChildApp -> IO ()
newBackend channel simReq storage childAppT = do
           _ <- spawnChildApp storage childAppT

--           forever ( .. )

           return ()