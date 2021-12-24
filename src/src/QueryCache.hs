module QueryCache
  (addDomainToCache, QueryCache.init, Model, QueryCache.find)
  where

import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import Control.Concurrent.MVar

data Model =
  Model
    { cache :: MVar [QueryStamp]
    }

data QueryStamp =
  QueryStamp
    { domain :: String
    , t :: UTCTime
    }
    deriving (Show)

init :: IO Model
init = do
  cache <- newMVar []
  return $ Model { cache = cache }

mintStamp :: String -> IO QueryStamp
mintStamp q = do
  t <- getCurrentTime
  return $ QueryStamp q t

find :: Model -> String -> IO (Maybe QueryStamp)
find model dom = do
  cache <- readMVar $ cache model
  return $ Data.List.find (\qs -> domain qs == dom) cache

addDomainToCache :: Model -> String -> IO ()
addDomainToCache model d = do
  stamp <- mintStamp d
  let cache_mvar = cache model
  cache_val <- takeMVar $ cache_mvar
  putMVar cache_mvar $ take 10 $ stamp:cache_val

