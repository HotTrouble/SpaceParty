module TcpServer
(
 server
) where

import Network (listenOn, PortID(PortNumber))
import Network.Socket hiding (recv)
import qualified Network.Socket.ByteString as NSB
import qualified Network.Socket.ByteString.Lazy as NSBL
import Network.Socket.ByteString (sendAll, recv)
import Data.ByteString.Lazy (ByteString, fromChunks)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

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

server :: String -> PortNumber -> (Socket -> IO()) -> IO()
server host port handleRequest = withSocketsDo $ do
        sock <- initSocket host port
        forever $ acceptAndProcess sock handleRequest
        sClose sock

initSocket :: String -> PortNumber -> IO(Socket)
initSocket host port = listenOn $ PortNumber port

acceptAndProcess :: Socket -> (Socket-> IO()) -> IO()
acceptAndProcess sock handleRequest = do
    (s, _) <- accept sock
    setSocketOption s NoDelay 1
    process handleRequest s

process :: (Socket -> IO()) -> Socket -> IO()
process handleRequest sock = do
        handleRequest sock
        sClose sock
        return ()
