{-# LANGUAGE OverloadedStrings #-}
module SlackHook
  (Model, SlackHook.init, push) where

import qualified Args
import qualified Data.Text as T
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.Text
import Text.URI
import Network.HTTP.Req

data Model =
  Model
    { hook :: String
    , monitor_domain :: Maybe String
    }

init :: Args.Args -> IO Model
init args = do
  webhook <- readFile "config" >>= return . T.unpack . T.strip . T.pack
  return $ Model
    { hook = webhook
    , monitor_domain = normalizeMonitorDomain $ Args.monitor_domain args
    }

normalizeMonitorDomain :: Maybe String -> Maybe String
normalizeMonitorDomain Nothing = Nothing
normalizeMonitorDomain (Just s) =
  Just $ before ++ s ++ after
  where
    before = case Prelude.head s == '.' of
      True -> ""
      False -> "."
    after = case Prelude.last s == '.' of
      True -> ""
      False -> "."

push :: Model -> String -> IO ()
push model domain = do
  let payload =
        object
          [ "text" .= ("`" ++ domain ++ "`" :: String)
          , "username" .= ("DNS Flare" :: String)
          ]
  case shouldRelayQuery model domain of
    False ->
      return ()
    True -> do
      uri <- mkURI $ Data.Text.pack $ hook model
      case useHttpsURI uri of
        Nothing -> do
          putStrLn "Failed to parse URI"
          return ()
        Just (url, option) -> do
          -- eres <- tryAny $ runReq defaultHttpConfig $ do
          runReq defaultHttpConfig $ do
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
          -- case eres of
          --   Left e ->
          --     putStrLn $ show e
          --   Right io ->
          --     return io



shouldRelayQuery :: Model -> String -> Bool
shouldRelayQuery model domain =
  fromMaybe True $ (\s -> s `isSuffixOf` (T.pack domain)) <$> T.pack <$> monitor_domain model
