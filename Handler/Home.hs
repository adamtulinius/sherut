{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import ProcessList
import Data.List (sortBy)


getHomeR :: Handler RepHtml
getHomeR = do
                childrenTVar <- processList <$> getYesod
                childrenT <- liftIO $ listChildApps childrenTVar
                children' <- liftIO $ atomically $ do
                         mapM readTVar childrenT

                let children = sortBy (\a b -> compare (identifyChild a)
                                                       (identifyChild b))
                                      children'

                defaultLayout $(widgetFile "children")


getNewProcessR :: Handler RepHtml
getNewProcessR = do
               portTVar <- usedPorts <$> getYesod
               storage <- processList <$> getYesod

               childT <- liftIO $ createChildApp portTVar storage "localhost" "/" "test-app2" "0.0.1"
                                                      "cabal-dev/bin/sherut"
                                                      [Just "Development", Just"--port", Nothing]

               child <- liftIO $ atomically $ readTVar childT

               defaultLayout $(widgetFile "child")


getViewProcessR :: String -> Handler RepHtml
getViewProcessR appId = do
                storage <- processList <$> getYesod
                mChildT <- liftIO $ atomically $ getChildById storage appId

                case mChildT of
                     Just childT -> do
                          child <- liftIO $ atomically $ readTVar childT
                          defaultLayout $(widgetFile "child")
                     Nothing -> notFound


getStartProcessR :: String -> Handler RepHtml
getStartProcessR appId = undefined


getKillProcessR :: String -> Handler ()
getKillProcessR appId = do
                storage <- processList <$> getYesod
                mChild <- liftIO $ killChildApp storage appId

                case mChild of
                     Nothing -> notFound
                     Just _ -> redirect HomeR


getRestartProcessR :: String -> Handler RepHtml
getRestartProcessR appId = undefined