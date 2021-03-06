{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module SpaceParty.Network.Commands
(
  processCommand
)
where

import Network.Socket
import Network.Socket.ByteString.Lazy (sendAll)
import Data.Word
import Data.Bits
import Data.Binary
import GHC.Generics
import Control.Monad (forever, guard)
import Control.Exception (tryJust)
import System.IO.Error (isEOFError)
import Data.Acid

import SpaceParty.World.Galaxy
import SpaceParty.World.Sectors
import SpaceParty.World.Ships

processCommand :: Char -> (AcidState Galaxy) -> Socket -> IO()
processCommand 'W' _ sock = whereCommand sock
processCommand 'O' galaxy sock = who galaxy sock
processCommand 'J' galaxy sock = join galaxy sock
processCommand 'A' galaxy sock = addShipCommand galaxy sock

data WhereResponse = WhereResponse Sector deriving (Generic, Show)
instance Binary WhereResponse where
  put (WhereResponse sector) = do
    put 'W'
    put sector

whereCommand :: Socket -> IO()
whereCommand sock = do
  (SockAddrInet port addr) <- getPeerName sock
  let sector = sectorForIP addr
  let resp = WhereResponse sector
  sendAll sock $ encode resp
  return ()

instance Binary Ship where
  put (Ship name captain engineer) = do
    put name
    put captain
    put engineer

data WhoResponse = WhoResponse [Ship] deriving (Generic, Show)
instance Binary WhoResponse where
  put (WhoResponse ships) = do
    let numShips = fromIntegral (length ships) :: Word8
    put 'O'
    put numShips
    mapM_ put ships

who :: (AcidState Galaxy) -> Socket -> IO()
who galaxy sock = do
  (SockAddrInet port addr) <- getPeerName sock
  let sector = sectorForIP addr
  ships <- query galaxy (GetShips sector)
  let resp = WhoResponse ships
  sendAll sock $ encode resp
  return ()

join :: (AcidState Galaxy) -> Socket -> IO()
join galaxy sock = do
  (SockAddrInet port addr) <- getPeerName sock
  let sector = sectorForIP addr
  let resp = WhoResponse []
  sendAll sock $ encode resp
  return ()

data AddShipResponse = AddShipResponse Ship deriving (Generic, Show)
instance Binary AddShipResponse where
  put (AddShipResponse ship) = do
    put 'A'
    put ship

addShipCommand :: (AcidState Galaxy) -> Socket -> IO()
addShipCommand galaxy sock = do
  (SockAddrInet port addr) <- getPeerName sock
  let sector = sectorForIP addr
  let resp = WhoResponse []
  sendAll sock $ encode resp
  return ()
