module Day22 (solve) where

import Data.List (sortBy)
import qualified Data.Set as S
import Data.Maybe (mapMaybe)

import Text.Regex.Applicative ( RE, sym, (=~), (<|>), string, psym )
import Text.Regex.Applicative.Common (decimal)
import Linear.V3 (V3 (..))

import Debug.Trace (trace)
dbg :: Show a => a -> a
dbg a = trace (show a) a

type Pos = V3 Int
data Brick = Brick Int Pos Pos deriving (Eq, Show)
instance Ord Brick where
  compare (Brick i (V3 a b c) _) (Brick j (V3 d e f) _)
    | a == d && b == e && c == f = compare i j
    | c == f && b == e = compare a d
    | c == f = compare b e
    | otherwise = compare c f
type Stack = S.Set Brick
type Parser a = RE Char a

type Inp = Stack

highPoint :: Brick -> Int
highPoint (Brick _ _ (V3 _ _ z)) = z

lowPoint :: Brick -> Int
lowPoint (Brick _ (V3 _ _ z) _) = z

collide :: Brick -> Brick -> Bool
collide (Brick _ (V3 a b _) (V3 c d _)) (Brick _ (V3 e f _) (V3 g h _))
  | a == c && b == d = e <= a && a <= g && f <= b && b <= h
  | b == d = f <= b && b <= h && (maxMinX <= minMaxX)
  | otherwise = e <= a && a <= g && (maxMinY <= minMaxY)
  where
    maxMinX = max a e
    minMaxX = min c g
    maxMinY = max b f
    minMaxY = min d h

supported :: Brick -> Brick -> Bool
supported brick otherBrick =
  lowPoint otherBrick == highPoint brick + 1 && collide brick otherBrick

isSupported :: Brick -> Brick -> Bool
isSupported brick otherBrick =
  highPoint otherBrick == lowPoint brick - 1 && collide brick otherBrick

fall :: Stack -> Brick -> Stack
fall bricks brick@(Brick iD (V3 a b c) (V3 d e f))
  | c == 1 || any (\b -> highPoint b == c - 1 && collide brick b) bricks =
    S.insert brick bricks
  | otherwise = fall bricks fallen
  where
    fallen = Brick iD (V3 a b (c - 1)) (V3 d e (f - 1))

supportingFilter :: Stack -> Brick -> Bool
supportingFilter stack brick =
  1 `elem`
  (foldl (\l ob -> (S.size . S.filter (isSupported ob) $ stack) : l) [] .
   S.filter (supported brick) $
   stack)

notSupporting :: Stack -> Stack
notSupporting stack = S.filter (not . supportingFilter stack) stack

part1 :: Inp -> Int
part1 stack = S.size . notSupporting $ stack
  where
    notSupporting = S.filter (not . supportingFilter stack)

disintegrate :: Stack -> Brick -> Stack
disintegrate bricks brick = foldl fall S.empty . S.delete brick $ bricks

supporting :: Stack -> Stack
supporting stack = S.filter (supportingFilter stack) stack

countFall :: Stack -> Brick -> Int
countFall stack brick =
  S.size . S.difference (disintegrate stack brick) . S.delete brick $ stack

countAllFall :: Stack -> Stack -> Int
countAllFall fullStack = foldr ((+) . countFall fullStack) 0

part2 :: Inp -> Int
part2 stack = countAllFall stack isSupport
  where
    isSupport = supporting stack

coord :: Parser Pos
coord = V3 <$> decimal <* sym ',' <*> decimal <* sym ',' <*> decimal

parse :: Parser (Pos, Pos)
parse = (,) <$> coord <* sym '~' <*> coord

solve :: String -> IO ()
solve input = putStrLn "--- Day 22 ---" >> print (part1 fallenStack) >> print (part2 fallenStack)
  where
    fallenStack =
      foldl fall S.empty .
      S.fromList .
      zipWith (curry toBrick) [1..] .
      mapMaybe (=~ parse) .
      lines $ input
    toBrick :: (Int, (Pos, Pos)) -> Brick
    toBrick (i, (p1, p2)) = Brick i p1 p2
