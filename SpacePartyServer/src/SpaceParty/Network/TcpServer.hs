module SpaceParty.Network.TcpServer
(
 server
)
where

import Network (PortID(PortNumber))
import Network.Socket hiding (recv)
import qualified Network.Socket.ByteString as NSB
import qualified Network.Socket.ByteString.Lazy as NSBL
import Network.Socket.ByteString (sendAll, recv)
import Data.ByteString.Lazy (ByteString, fromChunks)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Acid

import SpaceParty.World.Galaxy

type Host = SockAddr

recvAll :: Socket -> IO(BL.ByteString)
recvAll sock = do
    list <- recvList sock
    return (BL.fromChunks list)

recvList :: Socket -> IO([B.ByteString])
recvList sock = do
    input <- recv sock 4096
    if B.null input
        then return ([input])
        else do
            next <- recvList sock
            return (input:next)

server :: String -> PortNumber -> (AcidState Galaxy) -> ((AcidState Galaxy) -> Socket -> IO()) -> IO()
server host port galaxy handleRequest = withSocketsDo $ do
        sock <- initSocket host port
        forever $ acceptAndProcess sock galaxy handleRequest
        sClose sock

initSocket :: String -> PortNumber -> IO(Socket)
initSocket host port = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  haddr <- inet_addr host
  let addr = SockAddrInet port haddr
  bound <- bind sock addr
  listen sock 1
  return sock

acceptAndProcess :: Socket -> (AcidState Galaxy) -> ((AcidState Galaxy) -> Socket-> IO()) -> IO()
acceptAndProcess sock galaxy handleRequest = do
    (s, _) <- accept sock
    setSocketOption s NoDelay 1
    process handleRequest galaxy s

process :: ((AcidState Galaxy) -> Socket -> IO()) -> (AcidState Galaxy) -> Socket -> IO()
process handleRequest galaxy sock = do
        handleRequest galaxy sock
--        sClose sock
        return ()
