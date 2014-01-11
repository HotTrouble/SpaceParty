module Main (
    main
) where

import Network.Socket

import TcpServer

main :: IO()
main = do
  let host = "0.0.0.0"
  let port = 5847
  server host port process

process :: Socket -> IO()
process sock = do
  addr <- getPeerName sock
  sent <- send sock $ show addr
  --(SockAddrInet port addr)
  return ()

