{-# LANGUAGE OverloadedStrings #-}

module Misc
    ( ChildApp (..)
    , ChildAppState (..)
    , identifyChild
    , childRunning
    , getChildById
    , listChildApps
    , addChildApp
    , updateChildApp
    , deleteChildApp
    , matchChildAppIdT
    , setChildState
    , spawnChildApp
    , childWatcher
    , killChildApp
    ) where

import Prelude
import Data.Maybe (fromMaybe)
import System.Process
import GHC.Conc (atomically, STM, TVar, readTVar, writeTVar)
import Control.Concurrent (forkIO)
import Control.Monad (filterM)
import Proxy


data ChildApp = ChildApp { name :: String
                         , version :: String
                         , filePath :: FilePath
                         , args :: [Maybe String]
                         , portNumber :: Int
                         , handle :: Maybe ProcessHandle
                         , state :: ChildAppState
                         }


data ChildAppState = Started | Stopped | Starting | Stopping | Crashed
                     deriving (Show, Eq)


identifyChild :: ChildApp -> String
identifyChild childApp = name childApp
                         ++ "-"
                         ++ version childApp
                         ++ ":"
                         ++ (show $ portNumber childApp)


childRunning :: ChildApp -> IO Bool
childRunning childApp = do
             case (handle childApp) of
                  Nothing -> return False
                  Just processHandle -> do
                       mExitCode <- getProcessExitCode processHandle
                       case mExitCode of
                            Nothing -> return True
                            _       -> return False


getChildById :: TVar [TVar ChildApp] -> String -> STM (Maybe (TVar ChildApp))
getChildById storage needle = do
             children <- readTVar storage

             let hitsSTM = filterM (\this -> matchChildAppIdT needle this (==)) children
                 foo (hit:_) = Just hit
                 foo [] = Nothing

             hits <- hitsSTM

             return $ foo hits


listChildApps :: TVar [TVar ChildApp] -> IO [TVar ChildApp]
listChildApps storage =  do
              children <- atomically $ readTVar storage
              return children


addChildApp :: TVar [TVar ChildApp] -> TVar ChildApp -> STM ()
addChildApp tvar newChildT = do
            children <- readTVar tvar
            writeTVar tvar (newChildT:children)


updateChildApp :: TVar ChildApp -> ChildApp -> STM ()
updateChildApp destination childApp = do
               writeTVar destination childApp


matchChildAppIdT :: String -> TVar ChildApp -> (String -> String -> Bool) -> STM Bool
matchChildAppIdT childAppId childAppT f = do
                 childApp <- readTVar childAppT
                 return $ f childAppId $ identifyChild childApp


deleteChildApp :: TVar [TVar ChildApp] -> TVar ChildApp -> STM [TVar ChildApp]
deleteChildApp storage childAppT = do
               children <- readTVar storage

               childApp <- readTVar childAppT

               let childAppId = identifyChild childApp
                   childrenSTM = filterM (\c -> matchChildAppIdT childAppId c (/=)) children

               children' <- childrenSTM

               writeTVar storage children'
               return children'


setChildState :: TVar ChildApp -> ChildAppState -> STM ()
setChildState childAppT newState = do
              childApp <- readTVar childAppT
              updateChildApp childAppT
                             (ChildApp (name childApp)
                                       (version childApp)
                                       (filePath childApp)
                                       (args childApp)
                                       (portNumber childApp)
                                       (handle childApp)
                                       newState)


spawnChildApp :: TVar [TVar ChildApp] -> TVar ChildApp -> IO ()
spawnChildApp storage childAppT = do
              childApp <- atomically $ readTVar childAppT

              processHandle <- runProcess (filePath childApp) (fillArgs (args childApp)
                                                                        (portNumber childApp))
                                          Nothing Nothing Nothing Nothing Nothing

              -- FIXME: check whether the process actually started

              let newChild = ChildApp (name childApp)
                                      (version childApp)
                                      (filePath childApp)
                                      (args childApp)
                                      (portNumber childApp)
                                      (Just processHandle)
                                      Started

              _ <- atomically $ updateChildApp childAppT newChild
              _ <- forkIO $ childWatcher storage childAppT
              return ()


childWatcher :: TVar [TVar ChildApp] -> TVar ChildApp -> IO ()

childWatcher storage childAppT = do
             childApp <- atomically $ readTVar childAppT

             case (handle childApp) of
                  Nothing -> do
                          case (state childApp) of
                               Started -> do
                                       spawnChildApp storage childAppT
                                       return ()
                               _       -> do
                                       return ()

                  Just processHandle -> do
                       exitCode <- waitForProcess processHandle

                       childApp2 <- atomically $ readTVar childAppT

                       case state childApp2 of
                            Stopping -> do
                                     _ <- atomically $ deleteChildApp storage childAppT
                                     return ()
                            Started -> do
                                    _ <- spawnChildApp storage childAppT
                                    return ()
                            _ -> do
                              return ()

             return ()


killChildApp :: TVar [TVar ChildApp] -> String -> IO (Maybe ChildApp)
killChildApp storage childId = do
             mChildT <- atomically $ getChildById storage childId

             case mChildT of
                  Nothing -> return Nothing
                  Just childT -> do
                       child <- atomically $ readTVar childT
                       case (handle child) of
                            Nothing -> do
--                                    _ <- atomically $ setChildState storage child Stopped -- måske er dette nødvendigt, selvom childWatcher tråden burde gøre det
                                    return (Just child)
                            Just processHandle -> do
                                 _ <- atomically $ setChildState childT Stopping
                                 _ <- terminateProcess processHandle
                                 return (Just child)


fillArgs :: [Maybe String] -> Int -> [String]
fillArgs mArgs port' = map (fromMaybe (show port')) mArgs
