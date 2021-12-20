module Lib
    ( main
    ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Network.DNS.IO as DNS.IO
import qualified Network.DNS.Types as DNS
import           Network.DNS.Types (DNSMessage)
import qualified Network.DNS.Encode as DNS.Encode
import qualified Network.DNS.Decode as DNS.Decode
import Data.IP.Internal

main :: IO ()
main = runUDPServer Nothing "53" talk
  where
    talk s msg client  = do
        putStrLn $ show msg
        mapM_
          (\q ->
            DNS.IO.sendTo s
              (DNS.Encode.encode
                (DNS.IO.responseA
                  (DNS.identifier $ DNS.header msg)
                  q
                  [read "127.0.0.1" :: IPv4]
                )
              )
              client
          )
          (DNS.question msg)

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server sock) (const $ gracefulClose conn 5000)


runUDPServer :: Maybe HostName -> ServiceName -> (Socket -> DNSMessage -> SockAddr -> IO a) -> IO a
runUDPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Datagram
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        return sock
    loop sock = forever $ do
        (msg, peer) <- DNS.IO.receiveFrom sock
        server sock msg peer

