{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module SpaceParty.World.Galaxy
(
  Galaxy(..),
  addShip,
  getShip
)
where

import Data.Acid
import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Applicative                   ( (<$>) )
import Data.SafeCopy
import Data.Map (Map, lookup, update)
import qualified Data.Map as M

import SpaceParty.World.Sectors (Sector(..))
import SpaceParty.World.Ships (Ship(..))

data Galaxy = Galaxy (Map Sector [Ship])

$(deriveSafeCopy 0 'base ''Galaxy)

addShip :: Sector -> Ship -> Update Galaxy ()
addShip sector ship = do
  galaxy <- get
  let ships = lookup sector galaxy
  let ships' = ships ++ [ship]
  let galaxy' = update sector ships' galaxy
  put galaxy'

getShips :: Sector -> Query Galaxy [Ship]
getShips sector = do
  galaxy <- ask
  let ships = lookup sector galaxy
  return ships

$(makeAcidic ''Galaxy ['addShip, 'getShips])

{-
main :: IO ()
main = do
  args <- getArgs
  database <- openLocalStateFrom "myDatabase/" (Database ["Welcome to the acid-state database."])
  if null args
    then do
      messages <- query database (ViewMessages 10)
                    putStrLn "Last 10 messages:"
                    mapM_ putStrLn [ "  " ++ message | message <- messages ]
    else do
      update database (AddMessage (unwords args))
                    putStrLn "Yo
-}
