{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module SpaceParty.World.Sectors
(
  Sector(..)
)
where

import Data.Word
import Data.Bits
import Data.Binary
import GHC.Generics

data Sector = Sector Integer Integer deriving (Generic, Show) -- radius angle
instance Binary Sector where
  put (Sector r a) = do
    let r' = ((fromIntegral r)::Word16)
    let a' = ((fromIntegral a)::Word16)
    put $ swapEndian16 r'
    put $ swapEndian16 a'

sectorForIP :: Word32 -> Sector
sectorForIP addr =
  let coords@(r,a) = convertCoords $ split32 addr
  in Sector r a

split32 :: Word32 -> (Word16, Word16)
split32 w = (fromIntegral (w `shiftR` 16), fromIntegral w)

swapEndian16 :: Word16 -> Word16
swapEndian16 w =
  let (a,b) = split16 w
  in combine8 (b,a)

split16 :: Word16 -> (Word8, Word8)
split16 w = (fromIntegral (w `shiftR` 8), fromIntegral w)

combine8 :: (Word8, Word8) -> Word16
combine8 (a,b) =
  let a' = (fromIntegral a) :: Word16
      b' = (fromIntegral b) :: Word16
      big = a' `shiftL` 8
      little = b'
  in big+little

convertCoords :: (Word16, Word16) -> (Integer, Integer)
convertCoords (r,a) =
  let r' = fromIntegral $ (-1)*r `div` 2
      ss = shellSize r'
      a' = (((fromIntegral a)+r') `mod` (shellSize r')) `div` 2
      h = hextant (r', a')
      side = ss `div` 6
      hex = transform h side r' a'
      (x, y) = squareGrid hex
  in (x, y)

shellSize :: Integer -> Integer
shellSize 0 = 1
shellSize r = 6*r

hextant :: (Integer, Integer) -> Integer
hextant (r, a) =
  let ss   = shellSize r
      side = ss `div` 6
  in findSide a side 0

findSide :: Integer -> Integer -> Integer -> Integer
findSide a side count =
  if a < (side*(count+1))
    then count
    else findSide a side (count+1)

transform :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
transform 0 side r a = (r+a, a)
transform 1 side r a = (r+a, side)
transform 2 side r a = (r+a, side-a)
transform 3 side r a = (r+((side*3)-a), -a)
transform 4 side r a = (r+((side*3)-a), -side)
transform 5 side r a = (r+((side*3)-a), -(side-a))

squareGrid :: (Integer, Integer) -> (Integer, Integer)
squareGrid (hx, hy) =
  let x = hx*2
  in if (hy `mod` 2) == 0
    then (x+1, hy)
    else (x, hy)

