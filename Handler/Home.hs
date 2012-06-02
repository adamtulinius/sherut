{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import ProcessList


getHomeR :: Handler RepHtml
getHomeR = do
                childrenTVar <- processList <$> getYesod
                children <- liftIO $ listChildApps childrenTVar

                defaultLayout $(widgetFile "children")

getNewProcessR :: Handler RepHtml
getNewProcessR = do
               portTVar <- usedPorts <$> getYesod
               storage <- processList <$> getYesod

               child <- liftIO $ createChildApp portTVar storage "test-app2" "0.0.1"
                                                      "cabal-dev/bin/sherut"
                                                      [Just "Development", Just"--port", Nothing]


               defaultLayout $(widgetFile "child")


getViewProcessR :: String -> Handler RepHtml
getViewProcessR appId = do
                storage <- processList <$> getYesod
                mChild <- liftIO $ atomically $ getChildById storage appId

                case mChild of
                     Just child -> defaultLayout $(widgetFile "child")
                     Nothing -> notFound


getStartProcessR :: String -> Handler RepHtml
getStartProcessR appId = undefined


getKillProcessR :: String -> Handler ()
getKillProcessR appId = do
                storage <- processList <$> getYesod
                mChild <- liftIO $ atomically $ killChildApp storage appId

                case mChild of
                     Nothing -> notFound
                     Just _ -> redirect $ ViewProcessR appId



getRestartProcessR :: String -> Handler RepHtml
getRestartProcessR appId = undefined