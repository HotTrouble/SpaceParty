{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module SpaceParty.World.Galaxy where

import Data.Acid
import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Applicative                   ( (<$>) )
import Data.SafeCopy
import Data.Map (Map)
import qualified Data.Map as M

import SpaceParty.World.Sectors
import SpaceParty.World.Ships

data Galaxy = Galaxy (Map Sector [Ship])

$(deriveSafeCopy 0 'base ''Galaxy)
$(deriveSafeCopy 0 'base ''Ship)
$(deriveSafeCopy 0 'base ''Sector)

addShip :: Sector -> Ship -> Update Galaxy ()
addShip sector ship = do
  (Galaxy galaxy) <- get
  let maybeShips = M.lookup sector galaxy
  case maybeShips of
    Nothing -> do
      let ships' = [ship]
      let galaxy' = M.insert sector ships' galaxy
      put (Galaxy galaxy')
    Just ships -> do
      let ships' = ships ++ [ship]
      let galaxy' = M.insert sector ships' galaxy
      put (Galaxy galaxy')

getShips :: Sector -> Query Galaxy [Ship]
getShips sector = do
  (Galaxy galaxy) <- ask
  let maybeShips = M.lookup sector galaxy
  case maybeShips of
    Nothing    -> return []
    Just ships -> return ships

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
