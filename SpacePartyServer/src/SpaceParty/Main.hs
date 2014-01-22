{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module SpaceParty.Main
(
    main
) where

import Network.Socket
import Network.Socket.ByteString.Lazy (sendAll)
import Control.Monad (forever, guard)
import Control.Exception (tryJust)
import System.IO.Error (isEOFError)
import Data.Acid
import qualified Data.Map as M

import SpaceParty.Network.TcpServer
import SpaceParty.Network.Commands
import SpaceParty.World.Galaxy

main :: IO()
main = do
  let host = "0.0.0.0"
  let port = 5847

  galaxy <- openLocalStateFrom "galaxy/" (Galaxy M.empty)

  server host port galaxy process

process :: Galaxy -> Socket -> IO()
process galaxy sock = do
  putStrLn "Reading byte"
  eitherCommand <- tryJust (guard . isEOFError) $ recv sock 1
  case eitherCommand of
    Left _             -> do
      putStrLn "EOF"
      return ()
    Right (command:[]) -> do
      putStrLn "Read byte"
      processCommand command galaxy sock
      process galaxy sock -- recurse
