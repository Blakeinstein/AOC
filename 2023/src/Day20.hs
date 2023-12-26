{-# LANGUAGE TupleSections #-}
module Day20 (solve) where

import Control.Applicative (many)

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)


type Name = String
type Modules = M.Map Name Module
data Module = Flipflop Bool [Name] | Conjunction (M.Map Name PulseType) [Name] | Broadcast [Name] deriving (Eq, Show)
data PulseType = Low | High deriving (Eq, Show)
type Pulse = (Name, Name, PulseType)

type Inp = Modules

allHigh :: M.Map Name PulseType -> Bool
allHigh = all (==High) . M.elems


isConjunction :: Module -> Bool
isConjunction (Conjunction _ _) = True
isConjunction _ = False

flip' :: Name -> Modules -> Modules
flip' = M.adjust (\(Flipflop b ns) -> Flipflop (not b) ns)

run1 :: (Int, Int) -> [Pulse] -> Modules -> ((Int, Int), Modules)
run1 (nlo, nhi) [] ms = ((nlo, nhi), ms)
run1 (nlo, nhi)(p@(_,_,typ):pipeline) ms = run1 (nlo + (if typ==Low then 1 else 0), nhi + (if typ==High then 1 else 0)) (pipeline ++ ps) newMs
  where
    (newMs, ps) = runPulse p ms

count :: Int -> (Int, Int) -> Pulse -> Modules -> (Int, (Int, Int))
count n hl p ms
  | n==1000 = (n, (newl, newh))
  | otherwise = count (n+1) (newl, newh) p newms
  where
    ((newl, newh), newms) = run1 hl [p] ms

runPulse :: Pulse -> Modules -> (Modules, [Pulse])
runPulse (from, to, typ) ms
  | to `M.notMember` ms = (ms, [])
  | otherwise = 
      case toModule of
        Broadcast ns -> (ms, (to, ,Low) <$> ns)
        Flipflop False ns -> if typ == High then (ms, []) 
                                            else (flip' to ms, (to, ,High) <$> ns)
        Flipflop True ns -> if typ == High then (ms, []) 
                                          else (flip' to ms, (to, ,Low) <$> ns)
        Conjunction mp ns -> if allHigh newmp then (M.insert to (Conjunction newmp ns) ms, (to,,Low) <$> ns) 
                                              else (M.insert to (Conjunction newmp ns) ms, (to,,High) <$> ns) 
          where
            newmp = M.insert from typ mp
  where
    toModule = ms M.! to


run2 :: (Pulse -> Bool) -> [Pulse] -> Modules -> (Bool, Modules)
run2 _ [] modules = (False, modules)
run2 f (pulse:pipeline) modules
  | f pulse = (True, modules)
  | otherwise = run2 f (pipeline ++ newPulses) newModules
  where
    (newModules, newPulses) = runPulse pulse modules

runUntil :: Int -> (Pulse -> Bool) -> Pulse -> Modules -> Int
runUntil n f startPulse modules
  | b = n
  | otherwise = runUntil (n+1) f startPulse newModules
  where
    (b, newModules) = run2 f [startPulse] modules

part1 :: Inp -> Int
part1 g = (1000 `div` n)*(1000 `div` n)*l*h
  where 
    (n, (l,h)) = count 1 (0,0) ("button", "broadcaster", Low) g

part2 :: Inp -> Int
part2 g = foldl1 lcm ms
  where 
    names = ["kk", "vt", "xr", "fv"]
    ms = (\name -> runUntil 1 (\(from, to, typ) -> to=="sq" && from==name && typ ==High) ("button", "broadcaster", Low) g) <$> names

postProcess :: Modules -> Modules
postProcess mp = foldl (\acc (name, c@(Conjunction _ ns)) -> M.insert name (Conjunction (M.fromList $ (,Low) <$> getSenders name) ns) acc ) mp cs
  where
    cs = filter (isConjunction . snd) $ M.toList mp
    getSenders :: Name -> [Name]
    getSenders c = fst <$> filter (sendsTo c) (M.toList mp)
    sendsTo :: Name -> (Name, Module) -> Bool
    sendsTo c (_,e) = case e of
                        Flipflop _ ns -> c `elem` ns
                        Conjunction _ ns -> c `elem` ns
                        Broadcast ns -> c `elem` ns

parse :: [String] -> Inp
parse ls = postProcess $ M.fromList $ go <$> ls
  where
    go s
      | head (head ws) == '%' = (tail (head ws), Flipflop False ns)
      | head (head ws) == '&' = (tail (head ws), Conjunction M.empty ns)
      | head ws == "broadcaster" = (head ws, Broadcast ns)
      | otherwise = error $ "Error in parse of module: " ++ s
      where
        ws = words s
        ns = splitOn "," $ concat $ drop 2 ws

solve :: String -> IO ()
solve input = putStrLn "--- Day 20 ---" >> print (part1 g) >> print (part2 g)
  where
    g = parse . lines $ input
