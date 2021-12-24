{-# LANGUAGE OverloadedStrings #-}
module SlackHook
  (push) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import Text.URI
import Data.Text

push :: String -> String -> IO ()
push hookUri domain = do
  let payload =
        object
          [ "text" .= ("`" ++ domain ++ "`" :: String)
          , "username" .= ("DNS Flare" :: String)
          ]
  uri <- mkURI $ Data.Text.pack hookUri
  case useHttpsURI uri of
    Nothing -> do
      putStrLn "Failed to parse URI"
      return ()
    Just (url, option) -> runReq defaultHttpConfig $ do
      r <-
        req
          POST
          url
          (ReqBodyJson payload)
          ignoreResponse
          mempty
      liftIO $ case responseStatusCode r of
        200 ->
          return ()
        x ->
          putStrLn $ "Slack Failed with status code " ++ show x
