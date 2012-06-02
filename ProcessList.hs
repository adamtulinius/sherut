module ProcessList
    ( ChildApp (..)
    , ChildAppState (..)
    , identifyChild
    , getChildById
    , listChildApps
    , addChildApp
    , createChildApp
    , stopChildApp
    , module GHC.Conc
    , module System.Process
    ) where

import Prelude
import Data.Maybe (fromMaybe)
import Data.List (sort)
import System.Process
import GHC.Conc (atomically, STM, TVar, newTVar, readTVar, writeTVar, unsafeIOToSTM)



data ChildAppState = Started | Stopped | Starting | Stopping
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


getAvailablePort :: TVar [ChildApp] -> STM Int
getAvailablePort tvar = do
                 processList <- readTVar tvar
                 let usedPorts = map portNumber processList
                     sortedPorts = sort usedPorts
                     np []                     = 3001
                     np (x1:[])                = x1+1
                     np (x1:x2:xs) | diff <= 1 = np(x2:xs) 
                                   | diff  > 1 = x1+1
                        where diff = x2-x1
                     np _                      = 3001

                 return $ np sortedPorts



createChildApp :: TVar [ChildApp] -> String -> String
                -> FilePath -> [Maybe String] -> STM ChildApp

createChildApp tvar name' version' filePath' args' = do
               port <- getAvailablePort tvar
               let args'' = map (fromMaybe (show port)) args'
               processHandle <- unsafeIOToSTM $ runProcess filePath' args''
                                                           Nothing Nothing
                                                           Nothing Nothing
                                                           Nothing
               let newChild = ChildApp name' version' filePath' args'
                                       port (Just processHandle) Started
               _ <- addChildApp tvar newChild
               return newChild


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

stopChildApp :: TVar [ChildApp] -> String -> STM (Maybe ChildApp)
stopChildApp storage childId = do
             mChild <- getChildById storage childId

             case mChild of
                  Nothing -> return Nothing
                  Just child ->
                       case (handle child) of
                            Nothing -> return (Just child)
                            Just processHandle -> do
                                 _ <- unsafeIOToSTM $ terminateProcess processHandle
                                 return (Just child)


restartChild :: TVar [ChildApp] -> String -> STM (Maybe ChildApp)
restartChild storage childId = do
             stopChildApp storage childId
--             startChild storage childId
