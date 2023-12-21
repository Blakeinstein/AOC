module Day15 (solve) where

import Control.Applicative ( many, Alternative, some )

import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (transpose, sortBy, elemIndex)
import Data.Ord (comparing, Down(..))
import Data.Char (ord)

import Text.Regex.Applicative ( RE, sym, (=~), (<|>), string, psym )
import Debug.Trace (trace)

dbg :: Show a => a -> a
dbg a = trace (show a) a

sepBy :: Alternative f => f a -> f b -> f [a]
p `sepBy` sep = (:) <$> p <*> many (sep *> p)

type Parser a = RE Char a

step :: Int -> Char -> Int
step currValue char = ((currValue + ord char) * 17) `mod` 256

hash :: String -> Int
hash = foldl step 0

part1 :: [String] -> Int
part1 = sum . map hash

type Lens = (String, Int)
data Op = Remove String | Replace Lens
type Step = (Op, Int)

decode :: String -> Step
decode s = case break (`elem` "-=") s of
    (a, '-':b) -> (Remove a, hash a)
    (a, '=':b) -> (Replace (a, read b), hash a)

part2 :: [String] -> Int
part2 = power . foldl operate boxes . map decode
  where
    boxes = replicate 256 []
    operate bs = (`modify` bs)
    modify step = zipWith (modifyBox step) [0..]
    modifyBox (op, si) i box
      | si == i = modifyBox' op box
      | otherwise = box
    modifyBox' (Remove label) = filter ((/= label) . fst)
    modifyBox' (Replace lens) = reverse . append . foldl g (Just lens, [])
      where
        g (Nothing, box) lens' = (Nothing, lens' : box)
        g (Just lens, box) lens'
              | fst lens == fst lens' = (Nothing, lens : box)
              | otherwise = (Just lens, lens' : box)
        append (Nothing, box) = box
        append (Just lens, box) = lens : box
    power = sum . zipWith power' [1..]
    power' i = sum . zipWith (power'' i) [1..]
    power'' i j (_, v) = i * j * v

parse :: Parser [String]
parse = (some (psym (/= ',')) `sepBy` sym ',') <* sym '\n'

solve :: String -> IO ()
solve input = putStrLn "--- Day 15 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = fromMaybe (error "parse error") . (=~ parse)
