{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Day07 (solve) where

import Control.Applicative (Alternative, asum, many, some)

import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Text.Regex.Applicative (RE, sym, string, psym, (=~))
import Text.Regex.Applicative.Common (decimal)
import Debug.Trace (trace)

type Parser t = RE Char t

data Bid = Bid (Hand Value) Int deriving Show
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)

data Hand a = Hand a a a a a deriving (Functor, Foldable, Traversable, Show, Eq, Ord)
data Kind = HighCard
          | OnePair
          | TwoPair
          | ThreeOfAKind
          | FulLHouse
          | FourOfAKind
          | FiveOfAKind
          deriving (Show, Eq, Ord)


countElems :: (Ord a) => [a] -> M.Map a Int
countElems = M.fromListWith (+) . flip zip (repeat 1)

rankMap :: M.Map [Int] Kind
rankMap = M.fromList [ ([5], FiveOfAKind)
                     , ([4, 1], FourOfAKind)
                     , ([3, 2], FulLHouse)
                     , ([3, 1, 1], ThreeOfAKind)
                     , ([2, 2, 1], TwoPair)
                     , ([2, 1, 1, 1], OnePair)
                     , ([1, 1, 1, 1, 1], HighCard)
                     ]


kind :: Hand Value -> Kind
kind = (rankMap M.!) . sortOn negate . M.elems . countElems . toList

part1 :: [Bid] -> Int
part1 = sum . zipWith (*) [1..] . M.elems . M.fromList . map entry
  where entry (Bid hand bid)  = ((kind hand, hand), bid)

newtype JokerCard = JokerCard Value deriving (Show, Eq)
instance Ord JokerCard where
  JokerCard Jack <= JokerCard _ = True
  JokerCard _ <= JokerCard Jack = False
  JokerCard a <= JokerCard b = a <= b

jokerKind :: Hand JokerCard -> Kind
jokerKind hand = let freqs = countElems . toList $ hand
                     jokers = M.findWithDefault 0 (JokerCard Jack) freqs
                     others = M.elems (M.delete (JokerCard Jack) freqs)
                     (best:rest) = case sortOn negate others of
                                        [] -> [0]
                                        xs -> xs
                    in rankMap M.! ((best + jokers) : rest)

part2 :: [Bid] -> Int
part2  = sum . zipWith (*) [1..] . M.elems . M.fromList . map entry
  where entry (Bid hand bid)  = ((jokerKind hand', hand'), bid)
          where hand' = fmap JokerCard hand

value :: Parser Value
value = asum [ Two <$ sym '2', Three <$ sym '3', Four <$ sym '4', Five <$ sym '5'
             , Six <$ sym '6', Seven <$ sym '7', Eight <$ sym '8', Nine <$ sym '9'
             , Ten <$ sym 'T', Jack <$ sym 'J', Queen <$ sym 'Q', King <$ sym 'K', Ace <$ sym 'A'
             ]

handString :: Parser (Hand Value)
handString = Hand <$> value <*> value <*> value <*> value <*> value

parse :: Parser Bid
parse = Bid <$> (handString <* sym ' ') <*> decimal

solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = mapMaybe (=~ parse) . lines
