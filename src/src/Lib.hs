module Lib
    ( main
    ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Network.DNS.IO as DNS.IO
import qualified Network.DNS.Types as DNS
import           Network.DNS.Types (DNSMessage)
import qualified Network.DNS.Encode as DNS.Encode
import qualified Network.DNS.Decode as DNS.Decode
import Data.IP.Internal
import qualified SlackHook as Slack
import           Control.Concurrent.Async (concurrently)
import qualified QueryCache as QC
import           Args

data Model =
  Model
    { qc :: QC.Model
    , webhook :: String
    }

init :: Args -> IO Model
init args = do
  webhook <- readFile "config" >>= return . T.unpack . T.strip . T.pack
  qc <- QC.init $ cache_length args
  return $ Model
    { qc = qc
    , webhook = webhook
    }

main :: Args -> IO ()
main args@(ArgsServer {}) =
  Lib.init args >>= runServer args

runServer :: Args -> Model -> IO ()
runServer args model = do
  void $ concurrently
    (runUDPServer args model)
    (runTCPServer args model)

runTCPServer :: Args -> Model -> IO a
runTCPServer args model = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) Nothing (Just "53")
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        void $ forkFinally (runTCPServerClientThread args model conn peer) (const $ gracefulClose conn 5000)

runTCPServerClientThread :: Args -> Model -> Socket -> SockAddr -> IO ()
runTCPServerClientThread args model sock clientAddr = forever $ do
  msg <- DNS.IO.receiveVC sock
  msgHandler sock msg clientAddr
  where
    msgHandler s msg client  = do
        putStrLn $ show msg
        mapM_
          (\q -> do
            let
              domain = S8.unpack $ DNS.qname q
            doIfNotInCache model domain $ Slack.push (webhook model) domain
            DNS.IO.sendVC s
              (DNS.Encode.encode
                (DNS.IO.responseA (DNS.identifier $ DNS.header msg) q [])
              )
          )
          (DNS.question msg)

runUDPServer :: Args -> Model -> IO a
runUDPServer args model = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Datagram
              }
        head <$> getAddrInfo (Just hints) Nothing (Just "53")
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        return sock
    loop sock = forever $ do
        (msg, peer) <- DNS.IO.receiveFrom sock
        msgHandler sock msg peer
    msgHandler s msg client  = do
        putStrLn $ show msg
        mapM_
          (\q -> do
            let
              domain = S8.unpack $ DNS.qname q
            doIfNotInCache model domain $ Slack.push (webhook model) domain
            DNS.IO.sendTo s
              (DNS.Encode.encode
                (DNS.IO.responseA (DNS.identifier $ DNS.header msg) q [])
              )
              client
          )
          (DNS.question msg)


doIfNotInCache :: Model -> String -> IO() -> IO()
doIfNotInCache model domain io = do
  found <- QC.find (qc model) domain
  case found of
    Nothing -> do
      QC.addDomainToCache (qc model) domain
      io
    Just _ ->
      return ()
