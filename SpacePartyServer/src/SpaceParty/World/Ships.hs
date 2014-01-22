{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module SpaceParty.World.Ships
(
  Ship(..),
  shipNames
)
where

import Data.Binary
import GHC.Generics

data Ship = Ship String Bool Bool deriving (Generic, Show) -- name captain engineer
instance Binary Ship where
  put (Ship name captain engineer) = do
    put name
    put captain
    put engineer

shipNames :: [String]
shipNames = [
  "Maple Lady",
  "Senor Chupacabra",
  "Tamale Wagon",
  "Favorite Stepson",
  "Impossible Breakfast",
  "Community Teapot",
  "Discount Discotheque",
  "Spicy Meatball",
  "Smash Hulk"
 ]

