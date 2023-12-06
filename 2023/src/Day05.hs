{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
module Day05 (solve) where

import Control.Applicative (Alternative, asum, many, some)
import Control.Monad (void)

import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import Data.Char (toLower)
import Text.Regex.Applicative (RE, sym, string, psym, (=~))
import Text.Regex.Applicative.Common (decimal)
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))

type Resource = String
data DataRange = DataRange {_dest, _source, _size :: Int} deriving Show
type RangeMap = IM.IntMap DataRange
data MapRange = MapRange Resource Resource RangeMap deriving Show
type Index = M.Map Resource MapRange
data Alamanac = Alamanac [Int] Index deriving Show

type Parser t = RE Char t
spaces :: Parser ()
spaces = void (some (sym ' '))

seeds :: Parser [Int]
seeds = string "seeds:" *> some number
  where number = spaces *> decimal


toRangeMap :: [DataRange] -> RangeMap
toRangeMap = IM.fromList . map go
  where go r@(DataRange _ src _) = (src, r)

range :: Parser DataRange
range = DataRange <$> decimal <* spaces <*> decimal <* spaces <*> decimal

rangeMap :: Parser RangeMap
rangeMap = toRangeMap <$> some (range <* sym '\n')

resource :: Parser Resource
resource = some (psym (/= '-'))

mapRange :: Parser MapRange
mapRange = do
   sym '\n'
   src <- resource
   string "-to-"
   dest <- resource
   string " map:\n"
   range <- rangeMap
   pure (MapRange src dest range)

almanac :: Parser Alamanac
almanac = Alamanac <$> seeds   <* string "\n" <*> (collate <$> some mapRange)
  where collate ranges = M.fromList [(src, entry) | entry@(MapRange src _ _) <- ranges]


translate :: RangeMap -> Int -> Int
translate m idx = maybe idx go . IM.lookupLE idx $ m
  where go (_, DataRange dest source size) | source + size > idx = dest + (idx-source)
                                           | otherwise = idx

location :: Index -> Int -> Int
location idx = go "seed"
  where go ing n = maybe n handle (M.lookup ing idx)
          where handle (MapRange _ to next) = go to (translate next n)

part1 :: Alamanac -> Int
part1 (Alamanac seeds idx) = minimum . map (location idx) $ seeds

part2 :: Alamanac -> Int
part2 (Alamanac seeds idx) = makePairs seeds
  where go x y = minimum . map (location idx) $ [x..(x+y)]
        makePairs [] = maxBound
        makePairs (x:y:xs) = min (go x y) (makePairs xs)

solve :: String -> IO ()
solve input = putStrLn "--- Day 05 ---" >> print ( (part1 &&& part2) . p $ input)
  where p :: String -> Alamanac
        p = fromMaybe (error "no parse") . (=~ almanac)
