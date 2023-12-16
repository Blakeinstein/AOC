{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Day12 (solve) where

import Control.Applicative (Alternative, many)
import Control.Monad.State

import Data.List (nub, intercalate)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Text.Regex.Applicative ( RE, sym, (=~), (<|>) )
import Text.Regex.Applicative.Common (decimal, signed)
import Debug.Trace (trace)

sepBy :: Alternative f => f a -> f b -> f [a]
p `sepBy` sep = (:) <$> p <*> many (sep *> p)

type Parser a = RE Char a

data Cond = Op | Dam | Un deriving (Show, Eq, Ord)
type Rec = ([Cond], [Int])

type Inp = [Rec]

type Memo = M.Map ([Cond], [Int]) Int
type MemoState = State Memo Int

notAfter :: Int -> Cond -> [Cond] -> Bool
notAfter x c s = none c (take 1 (drop x s))

only :: Cond -> [Cond] -> Bool
only c = all (== c) . nub

none :: Cond -> [Cond] -> Bool
none c = notElem c . nub

possible' :: ([Cond] -> [Int] -> MemoState) -> [Cond] -> [Int] -> MemoState
possible' f [] [] = pure 1
possible' f [] [x] = pure 0
possible' f s [] = pure (if none Dam s then 1 else 0)
possible' f (Op:rs) xs = f rs xs
possible' f (Un:rs) xs = f rs xs >>= \a -> f (Dam:rs) xs >>= \b -> pure (a + b)
possible' f s (x:rx) | length s >= x && none Op (take x s) && notAfter x Dam s
  = f (drop (x + 1) s) rx
possible' _ _ _= pure 0

possible :: [Cond] -> [Int] -> Int
possible conds = flip evalState M.empty . memo possible' conds 
  where 
    memo mf s xs = let key = (s, xs) in gets (M.lookup key) >>= \case
      Just v -> pure v
      Nothing -> mf (memo mf) s xs >>= \v -> modify (M.insert key v) >> pure v

part1 :: Inp -> Int
part1 = sum . map (uncurry possible)

unfold :: Inp -> Inp
unfold = map f
  where f (s, xs) = (intercalate [Un] (replicate 5 s), concat (replicate 5 xs))

part2 :: Inp -> Int
part2 = part1 . unfold

conditions :: Parser [Cond]
conditions = many $ (Op <$ sym '.') <|> (Dam <$ sym '#') <|> (Un <$ sym '?')

damages :: Parser [Int]
damages = decimal `sepBy` sym ','

parse :: Parser Rec
parse = (,) <$> (conditions <* sym ' ') <*> damages

solve :: String -> IO ()
solve input = putStrLn "--- Day 12 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = mapMaybe (=~ parse) . lines
