module ProcessList
    ( ChildApp (..)
    , ChildAppState (..)
    , identifyChild
    , getChildById
    , childRunning
    , listChildApps
    , addChildApp
    , updateChildApp
    , setChildState
    , createChildApp
    , killChildApp
    , module GHC.Conc
    , module System.Process
    ) where

import Prelude
import Data.Maybe (fromMaybe)
import Data.List (sort)
import System.Process
import GHC.Conc (atomically, STM, TVar, newTVar, readTVar, writeTVar, unsafeIOToSTM)
import Control.Concurrent (forkIO)



data ChildAppState = Started | Stopped | Starting | Stopping | Crashed
                     deriving (Show)


data ChildApp = ChildApp { name :: String
                         , version :: String
                         , filePath :: FilePath
                         , args :: [Maybe String]
                         , portNumber :: Int
                         , handle :: Maybe ProcessHandle
                         , state :: ChildAppState
                         }

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


getChildById :: TVar [ChildApp] -> String -> STM (Maybe ChildApp)
getChildById storage needle = do
             children <- readTVar storage
             let hits = filter (\child -> needle == identifyChild child) children
                 foo (hit:_) = Just hit
                 foo [] = Nothing

             return $ foo hits

{-
getChildById' :: TVar [ChildApp] -> String -> STM (Maybe ChildApp, [ChildApp])
getChildById' storage needle = do
             children <- readTVar storage
             let (hits, misses) = partition (\child -> needle == childId child) children
                 foo (hit:_) = Just hit
                 foo [] = Nothing

             return $ (foo hits, misses)
-}

listChildApps :: TVar [ChildApp] -> IO [ChildApp]
listChildApps storage =  do
              children <- atomically $ readTVar storage
              return children


addChildApp :: TVar [ChildApp] -> ChildApp -> STM [ChildApp]
addChildApp tvar newChild = do
            children <- readTVar tvar
            let children' = (newChild: children)
            writeTVar tvar children'
            return children


updateChildApp :: TVar [ChildApp] -> ChildApp -> STM [ChildApp]
updateChildApp storage childApp = do
               children <- readTVar storage

               let childAppId = identifyChild childApp
                   children' = filter (\c -> childAppId /= (identifyChild c)) children
                   newChildren = (childApp:children')

               writeTVar storage newChildren
               return newChildren


setChildState :: TVar [ChildApp] -> ChildApp -> ChildAppState -> IO [ChildApp]
setChildState storage childApp newState = do
              let newChildApp = ChildApp (name childApp)
                                         (version childApp)
                                         (filePath childApp)
                                         (args childApp)
                                         (portNumber childApp)
                                         (handle childApp)
                                         newState

              children <- atomically $ updateChildApp storage newChildApp
              return children

getAvailablePort :: TVar [Int] -> STM Int
getAvailablePort tvar = do
                 usedPorts <- readTVar tvar

                 let sortedPorts = sort usedPorts
                     np []                     = 3001
                     np (x1:[])                = x1+1
                     np (x1:x2:xs) | diff <= 1 = np(x2:xs) 
                                   | diff  > 1 = x1+1
                        where diff = x2-x1
                     np _                      = 3001
                     newPort = np sortedPorts

                 _ <- writeTVar tvar (newPort:usedPorts)

                 return newPort



createChildApp :: TVar [Int] -> TVar [ChildApp] -> String -> String
                -> FilePath -> [Maybe String] -> IO ChildApp

createChildApp portsTVar storage name' version' filePath' args' = do
               port <- atomically $ getAvailablePort portsTVar
               let args'' = map (fromMaybe (show port)) args'
               processHandle <- runProcess filePath' args''
                                                           Nothing Nothing
                                                           Nothing Nothing
                                                           Nothing

               let newChild = ChildApp name' version' filePath' args'
                                       port (Just processHandle) Started

               _ <- forkIO $ childWatcher storage newChild -- FIXME: Save TheadId somewhere

               _ <- atomically $ addChildApp storage newChild
               return newChild


childWatcher :: TVar [ChildApp] -> ChildApp -> IO ()
childWatcher storage childApp = do
             case (handle childApp) of
--                  Nothing -> -- restart
                  Just processHandle -> do
                       _ <- waitForProcess processHandle
                       return ()

{-
startChild :: TVar [ChildApp] -> String -> STM (Maybe ChildApp)
startChild storage childId = do
           (mChild, children) <- getChildById storage childId

           case mChild of
                Nothing -> return Nothing
                Just child -> do
                     case (handle child) of
                          Nothing -> do
                          Just childHandle ->
-}

killChildApp :: TVar [ChildApp] -> String -> STM (Maybe ChildApp)
killChildApp storage childId = do
             mChild <- getChildById storage childId

             case mChild of
                  Nothing -> return Nothing
                  Just child ->
                       case (handle child) of
                            Nothing -> return (Just child)
                            Just processHandle -> do
                                 _ <- unsafeIOToSTM $ terminateProcess processHandle
--                                 _ <- unsafeIOToSTM $ waitForProcess processHandle
                                 return (Just child)


restartChild :: TVar [ChildApp] -> String -> STM (Maybe ChildApp)
restartChild storage childId = do
             killChildApp storage childId
--             startChild storage childId
