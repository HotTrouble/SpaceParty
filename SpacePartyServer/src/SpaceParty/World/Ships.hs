{-# LANGUAGE DeriveGeneric, DefaultSignatures, DeriveDataTypeable #-}

module SpaceParty.World.Ships
(
  Ship(..),
  Board(..),
  shipNames
)
where

import Data.Binary
import Data.ByteString.Lazy (fromChunks)
import GHC.Generics
import Data.Typeable
import Dust.Crypto.PRNG

data Ship = Ship String Bool Bool deriving (Generic, Show, Typeable) -- name captain engineer

data Board = Board DustGen [Ship] [Ship] [Ship] -- deck hand discards

draw :: Board -> Board
draw (Board gen (card:deck) hand discards) = Board gen deck (card:hand) discards

discard :: Ship -> Board -> Board
discard ship (Board gen deck hand discards) = Board gen deck hand discards -- FIXME

shuffleDiscards :: Board -> Board
shuffleDiscards (Board gen card hand discards) =
  let (shuffled, gen') = shuffleDeck discards gen
  in Board gen' shuffled hand []

shuffleDeck :: [Ship] -> DustGen -> ([Ship], DustGen)
shuffleDeck (card:[]) gen = ([card], gen)
shuffleDeck deck  gen =
  let l = length deck
      (bytes, gen') = randomBytes 2 gen
      w16 = (decode (fromChunks [bytes])) :: Word16
      index = (fromIntegral w16) :: Int
      card = deck !! index
      rest = drop (index+1) deck
      (shuffledRest, gen'') = shuffleDeck rest gen'
  in (card : shuffledRest, gen'')

deal :: IO Board
deal = do
  gen <- newDustGen
  let (shuffled, gen') = shuffleDeck makeDeck gen
  return $ Board gen' shuffled [] []

makeDeck :: [Ship]
makeDeck = map newShip shipNames

newShip :: String -> Ship
newShip name = Ship name False False

shipNames :: [String]
shipNames = [
  "Maple Lady",
  "Senior Chupacabra",
  "Tamale Wagon",
  "Favorite Stepson",
  "Impossible Breakfast",
  "Community Teapot",
  "Discount Discotheque",
  "Spicy Meatball",
  "Smash Hulk",
  "Baby Boba"
 ]

