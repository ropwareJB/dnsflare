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

main :: IO ()
main = do
  slackHook <- readFile "config" >>= return . T.unpack . T.strip . T.pack
  runServer slackHook

runServer :: String -> IO ()
runServer slackHook = do
  void $ concurrently
    (runUDPServer slackHook)
    (runTCPServer slackHook)

runTCPServer :: String -> IO a
runTCPServer slackHook = withSocketsDo $ do
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
        void $ forkFinally (runTCPServerClientThread slackHook conn peer) (const $ gracefulClose conn 5000)

runTCPServerClientThread :: String -> Socket -> SockAddr -> IO ()
runTCPServerClientThread slackHook sock clientAddr = forever $ do
  msg <- DNS.IO.receiveVC sock
  msgHandler sock msg clientAddr
  where
    msgHandler s msg client  = do
        putStrLn $ show msg
        mapM_
          (\q -> do
            Slack.push slackHook (S8.unpack $ DNS.qname q)
            DNS.IO.sendVC s
              (DNS.Encode.encode
                (DNS.IO.responseA (DNS.identifier $ DNS.header msg) q [])
              )
          )
          (DNS.question msg)

runUDPServer :: String -> IO a
runUDPServer slackHook = withSocketsDo $ do
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
            Slack.push slackHook (S8.unpack $ DNS.qname q)
            DNS.IO.sendTo s
              (DNS.Encode.encode
                (DNS.IO.responseA (DNS.identifier $ DNS.header msg) q [])
              )
              client
          )
          (DNS.question msg)

