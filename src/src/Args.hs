{-# LANGUAGE DeriveDataTypeable #-}
module Args
  (Args(..)) where

import           System.Console.CmdArgs

data Args =
  ArgsServer
    { cache_length :: Int
    }
  deriving (Show, Data, Typeable)

