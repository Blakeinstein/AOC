{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Coord where

import GHC.Generics (Generic, Generic1, Generically1)
import Data.Data (Data)
import Data.Ix (Ix)

data Coord = Coord {_y, _x :: Int} deriving (Show, Eq, Ord, Ix, Generic, Data)

mapCoord :: (Int -> Int) -> Coord -> Coord
mapCoord f (Coord y x) = Coord (f y) (f x)

zipCoord :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
zipCoord f (Coord y1 x1) (Coord y2 x2) = Coord (f y1 y2) (f x1 x2)

instance Num Coord where
  (+) = zipCoord (+)
  (-) = zipCoord (-)
  (*) = zipCoord (*)
  negate = mapCoord negate
  abs = mapCoord abs
  signum = mapCoord signum
  fromInteger = (\i -> Coord i i) . fromInteger

getY :: Coord -> Int
getY (Coord y _) = y

getX :: Coord -> Int
getX (Coord _ x) = x

_sum :: Coord -> Int
_sum (Coord y x) = y + x

distance :: Coord -> Coord -> Int
distance a b = _sum (abs ((-) a b))

cardinal :: Coord -> [Coord]
cardinal (Coord x y) = [Coord (y-1) x, Coord y (x-1), Coord y (x+1), Coord (y+1) x]

north :: Coord
north = Coord (-1) 0

east :: Coord
east = Coord 0 1

south :: Coord
south = Coord 1 0

west :: Coord
west = Coord 0 (-1)

invert :: Coord -> Coord
invert (Coord y x) = Coord x y

invert' :: Coord -> Coord
invert' (Coord y x) = Coord (-x) (-y)

data Dir = U | R | D | L | None deriving (Show, Eq, Ord)

(#+) :: Coord -> Dir -> Coord
c #+ dir = case dir of
  U -> c + north
  R -> c + east
  D -> c + south
  L -> c + west
  None -> c