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
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.Process
import GHC.Conc (atomically, STM, TVar, newTVar, readTVar, writeTVar, unsafeIOToSTM)
import Control.Concurrent (forkIO)



data ChildAppState = Started | Stopped | Starting | Stopping | Crashed
                     deriving (Show, Eq)


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


deleteChildApp :: TVar [ChildApp] -> ChildApp -> STM [ChildApp]
deleteChildApp storage childApp = do
               children <- readTVar storage

               let childAppId = identifyChild childApp
                   children' = filter (\c -> childAppId /= (identifyChild c)) children

               writeTVar storage children'
               return children'


setChildState :: TVar [ChildApp] -> ChildApp -> ChildAppState -> STM [ChildApp]
setChildState storage childApp newState = do
              let newChildApp = ChildApp (name childApp)
                                         (version childApp)
                                         (filePath childApp)
                                         (args childApp)
                                         (portNumber childApp)
                                         (handle childApp)
                                         newState

              updateChildApp storage newChildApp


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

               let newChild = ChildApp name' version' filePath' args'
                                       port Nothing Started

               _ <- atomically $ addChildApp storage newChild
               _ <- spawnChildApp storage newChild

               return newChild


fillArgs :: [Maybe String] -> Int -> [String]
fillArgs mArgs port' = map (fromMaybe (show port')) mArgs


spawnChildApp :: TVar [ChildApp] -> ChildApp -> IO ()
spawnChildApp storage (ChildApp name' version' filePath'
                               args' portNumber' handle'
                               state') = do

              processHandle <- runProcess filePath' (fillArgs args' portNumber')
                                          Nothing Nothing Nothing Nothing Nothing
              let newChild = ChildApp name' version' filePath' args'
                                      portNumber' (Just processHandle) Started
              _ <- atomically $ updateChildApp storage newChild
              _ <- forkIO $ childWatcher storage newChild
              return ()

--spawnChildAppAndClean :: TVar [Children] -> ChildApp -> IO ()
--spawnChildAppAndClean storage

-- todo: der tilføjes to entries til processList i stedet for en
-- set state=Stopping before kill for at undgå respawn

{-
childWatcher :: TVar [ChildApp] -> ChildApp -> IO ()
childWatcher storage childApp = do
             let childState = state childApp
             case (handle childApp) of
                  Nothing -> do
                          case (not $ childState `elem` [Stopped, Starting, Stopping]) of
                               True -> do
                                    _ <- spawnChildApp storage childApp
                                    return ()
                               False -> do
                                     return ()

                  Just processHandle -> do
                       exitCode <- waitForProcess processHandle
                       case exitCode of
                            ExitSuccess -> do
                                mChild <- atomically $ getChildById storage $ identifyChild childApp
                                case mChild of
                                     Nothing -> do
--                                             _ <- spawnChildApp storage childApp -- "Af en eller anden grund er der allerede ryddet op.."
                                             return ()
                                     Just child -> do
                                          case (state child) of
                                               Stopping -> do
                                                    _ <- atomically $ deleteChildApp storage child
                                                    return ()
                                               _ -> do
                                                    _ <- spawnChildApp storage childApp
                                                    return ()
                            ExitFailure errorCode -> do
                                        -- FIXME: log the errorCode somewhere
                                        _ <- spawnChildApp storage childApp
                                        return ()
                       return ()
-}


childWatcher :: TVar [ChildApp] -> ChildApp -> IO ()

childWatcher storage childApp@(ChildApp _ _ _ _ _ Nothing state')
             | state' `elem` [Stopped, Starting, Stopped] = spawnChildApp storage childApp

childWatcher storage childApp@(ChildApp _ _ _ _ _ (Just processHandle) state') = do
             exitCode <- waitForProcess processHandle
             mChild <- atomically $ getChildById storage $ identifyChild childApp

             case mChild of
                  Nothing -> do
                          return ()
                      --  Af en eller anden grund er der allerede ryddet op, dette bør nok logges.
                  Just child -> do
                       case state' of
                            Stopping -> do
                                     _ <- atomically $ deleteChildApp storage child
                                     return ()
                            Started -> do
--                              _ <- spawnChildApp storage childApp
                              return ()
                            _ -> do
                              return ()

             return ()

childWatcher _ _ = return ()


killChildApp :: TVar [ChildApp] -> String -> IO (Maybe ChildApp)
killChildApp storage childId = do
             mChild <- atomically $ getChildById storage childId

             case mChild of
                  Nothing -> return Nothing
                  Just child ->
                       case (handle child) of
                            Nothing -> do
--                                    _ <- atomically $ setChildState storage child Stopped -- måske er dette nødvendigt, selvom childWatcher tråden burde gøre dette
                                    return (Just child)
                            Just processHandle -> do
                                 _ <- atomically $ setChildState storage child Stopping
                                 _ <- terminateProcess processHandle
                                 return (Just child)


restartChild :: TVar [ChildApp] -> String -> IO (Maybe ChildApp)
restartChild storage childId = do
             killChildApp storage childId
--             startChild storage childId
