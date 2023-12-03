{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module Day03 (solve) where

import Control.Applicative (asum, many, some)
import Control.Monad (foldM_)
import Control.Monad.Trans.Writer (execWriter, tell)

import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Regex.Applicative (RE, sym, psym, (=~))

data Loc = Loc {x, y :: Int} deriving (Show, Eq, Ord)
data Located t = Located (S.Set Loc) t deriving (Show, Eq, Ord, Functor)
newtype Symbol = Symbol Char deriving (Show, Eq, Ord)
data Token = TokInt Int | TokBlank | TokSym Symbol deriving (Show, Eq, Ord)

type Inp = (S.Set (Located Int), M.Map Loc Symbol)

type Parser t = RE Char t

data Measured t = Measured Int t deriving (Show, Functor)
number, blank, symbol :: Parser (Measured Token)
number = do
  region <- some (psym isDigit)
  pure (Measured (length region) . TokInt . read $ region)
blank = Measured 1 TokBlank <$ sym '.'
symbol = Measured 1 . TokSym . Symbol <$> psym (not . ((||) <$> (== '.') <*> isDigit))

line :: Parser [Measured Token]
line = many (asum [number, blank, symbol])

collate :: [[Measured Token]] -> Inp
collate = execWriter . traverse_ row . zip [0..]
  where row (y, r) = foldM_ include 0 r
          where include x (Measured width tok) = do
                  case tok of
                    TokBlank -> pure ()
                    TokSym s -> tell (mempty, M.singleton (Loc x y) s)
                    TokInt i -> tell (S.singleton deflated, mempty)
                      where deflated = Located coords i
                            coords = S.fromList $ do
                              dx <- [0..width-1]
                              pure $ Loc (x + dx) y
                  pure $ x + width

neighbors :: Loc -> S.Set Loc
neighbors (Loc x y) = S.fromList $ do
  dx <- [-1..1]
  dy <- [-1..1]
  pure $ Loc (x+dx) (y+dy)

part1 :: Inp -> Int
part1 (nums, syms) = sum . mapMaybe part . S.toList $ nums
  where symLocs = M.keysSet syms
        part (Located loc s) | S.null $ S.intersection symLocs (foldMap neighbors loc) = Nothing
                             | otherwise = Just s

part2 :: Inp -> Int
part2 (nums, syms) = sum . mapMaybe ratio . M.assocs $ syms
  where nums' = S.toList nums
        ratio (loc, Symbol '*') = case filter adjacent nums' of
          [Located _ x, Located _ y] -> Just $ x * y
          _ -> Nothing
          where adjacent (Located coords _) = S.member loc (foldMap neighbors coords)
        ratio _ = Nothing

solve :: String -> IO ()
solve input = putStrLn "--- Day 03 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p :: String -> Inp
    p = collate . mapMaybe (=~ line) .lines
