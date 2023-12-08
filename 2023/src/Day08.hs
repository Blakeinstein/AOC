module Day08 (solve) where

import Control.Applicative (Alternative, asum, many, some)
import Control.Monad (void, replicateM)

import Data.Char (isDigit, isAlphaNum)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Regex.Applicative (RE, sym, string, psym, (=~), anySym, few)
import Text.Regex.Applicative.Common (decimal)
import Debug.Trace (trace)
import Data.Sequence (replicateA)
import Data.List (isSuffixOf)

data Direction = L | R deriving (Show, Eq)
data Node = Node {_left, _right :: String} deriving (Show)
type Map = M.Map String Node
data Docs = Docs [Direction] Map deriving (Show)

type Parser a = RE Char a

data Position = Position String Int

walk :: Docs -> String -> [String]
walk (Docs dirs map) start= scanl go start $ cycle dirs
  where go curr dir = let (Node left right) = map M.! curr
                      in case dir of
                        L -> left
                        R -> right


part1 :: Docs -> Int
part1 (Docs dirs map) = traverse (Position "AAA" 0)
  where dirsLen = length dirs
        traverse :: Position -> Int
        traverse (Position curr steps) | curr == "ZZZ" = steps
                                       | otherwise = traverse (Position next (steps + 1))
              where next = let (Node left right) = map M.! curr
                            in case currDirection of
                                      L -> left
                                      R -> right
                    currDirection = dirs!!(steps `mod` dirsLen)


data Position2 = Position2 [String] Int

endState :: [String] -> Bool
endState = all (\str -> last str == 'Z')

part2 :: Docs -> Int
part2 inp@(Docs dirs map) = let starts = filter (endsWith "A") . M.keys $ map
                            in foldr (lcm . length . takeWhile (not . endsWith "Z") . walk inp) 1 starts
        where endsWith s n = s `isSuffixOf` n

location :: Parser String
location = replicateM 3 (psym isAlphaNum)

node :: Parser Node
node = Node <$> (sym '(' *> location <* string ", " ) <*> location <* string ")"

mapTree :: Parser (String, Node)
mapTree = (,) <$> (sym '\n' *> location <* string " = ") <*> node

direction :: Parser Direction
direction = asum [ L <$ sym 'L', R <$ sym 'R' ]

directions :: Parser [Direction]
directions = some direction <* sym '\n'

parse :: Parser Docs
parse = Docs <$> directions <*> (collate <$> some mapTree) <* sym '\n'
  where collate = M.fromList

solve :: String -> IO ()
solve input = putStrLn "--- Day 08 ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = fromMaybe (error "no parse") . (=~ parse)
