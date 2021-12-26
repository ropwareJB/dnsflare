{-# LANGUAGE DeriveDataTypeable #-}
module Args
  (Args(..)) where

import           System.Console.CmdArgs

data Args =
  ArgsServer
    { cache_length :: Int
    , monitor_domain :: Maybe String
    }
  deriving (Show, Data, Typeable)

