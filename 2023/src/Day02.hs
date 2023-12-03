{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}

module Day02 (solve) where

import Control.Applicative (Alternative, asum, many)

import Data.Maybe (mapMaybe)
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import Text.Regex.Applicative (RE, sym, string, (=~))
import Text.Regex.Applicative.Common (decimal)

data Color = Red | Green | Blue deriving (Show, Eq, Ord, Enum, Bounded)
type Pull = M.Map Color Int
data Game a = Game Int a deriving (Show, Functor)
type Inp = [Game [Pull]]

combine :: Game [Pull] -> Game Pull
combine = fmap (M.unionsWith max)

part1 :: Inp -> Int
part1 = sum . mapMaybe (score . combine)
  where score (Game id p) | M.unionWith max p limit == limit = Just id
                          | otherwise = Nothing
        limit = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]

part2 :: Inp -> Int
part2 = sum . map (power . combine)
  where power (Game _ p) = product (M.elems p)

-- Input regex
type Parser exp = RE Char exp

space :: Parser ()
space = () <$ sym ' '

sepBy :: Alternative f => f a -> f b -> f [a]
p `sepBy` sep = (:) <$> p <*> many (sep *> p)

color :: Parser Color
color = asum [c <$ string (map toLower (show c)) | c <- [minBound..maxBound]]

oneColorPull :: Parser Pull
oneColorPull = flip M.singleton <$> decimal <* space <*> color

pull :: Parser Pull
pull = M.unionsWith (+) <$> oneColorPull `sepBy` string ", "

game :: Parser (Game [Pull])
game = Game <$> (string "Game " *> decimal) <* string ": " <*> pull `sepBy` string "; "

solve :: String -> IO ()
solve input = putStrLn "--- Day 02 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p :: String -> Inp
    p = mapMaybe (=~ game) . lines
