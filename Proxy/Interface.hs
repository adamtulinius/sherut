{-# LANGUAGE OverloadedStrings #-}

module Proxy.Interface
       ( newBackend
       , newBackendChannel
       ) where

import Prelude
import Yesod (liftIO)
import GHC.Conc (atomically, STM, TVar, newTVar, readTVar, writeTVar)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Concurrent.STM.TBMChan
import Control.Monad (forever)
import Control.Monad.STM
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Conduit as HC -- (Manager, Response, httpLbs)
import qualified Network.Wai as Wai
import qualified Data.Conduit as Conduit
import qualified Data.ByteString.Lazy.Char8 as LB
import Proxy
import Misc


type BackendChan =
     TBMChan ( (Wai.Request, (Address, Int))
             , MVar (H.Status, H.ResponseHeaders, LB.ByteString)
             )


newBackendChannel :: Int ->  STM (BackendChan)
newBackendChannel = newTBMChan


modTVar :: TVar Int -> (Int -> Int) -> STM Int
modTVar tvar f = do
               c <- readTVar tvar
               let newValue = f c
               writeTVar tvar newValue
               return newValue


fetch :: HC.Manager -> TVar Int
      -> MVar(H.Status, H.ResponseHeaders, LB.ByteString)
      -> HC.Request IO -> Conduit.ResourceT IO ()
fetch manager connectionCount responseMVar request = do
      _ <- liftIO $ atomically $ modTVar connectionCount (+ 1)
      HC.Response status _ headers src <- liftIO $ HC.httpLbs request manager
      _ <- liftIO $ putMVar responseMVar (status, headers, src)
      _ <- liftIO $ atomically $ modTVar connectionCount (1 -)
      return ()


newBackend :: HC.Manager -> BackendChan -> Int -> TVar [TVar ChildApp] -> TVar ChildApp -> IO ()
newBackend manager channel simReq storage childAppT = do
           _ <- spawnChildApp storage childAppT

           connectionCount <- atomically $ newTVar (0 :: Int)

           forever $ do
                   io <- atomically $ readTBMChan channel
                   case io of
                        Nothing -> return ()
                        Just ((request, url), responseMVar) -> do
                             let backendRequest = createBackendRequest request url
                             _ <- forkIO $ liftIO $
                                  fetch manager connectionCount responseMVar backendRequest
                             return ()

                   return ()