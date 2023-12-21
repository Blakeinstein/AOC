{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Day19 (solve) where

import Control.Lens (over)
import Control.Monad (replicateM_)
import Control.Applicative ( Alternative, many, some, asum )


import Linear.V2 (V2(..), _x, _y)
import Data.Char (isLower)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as M

import Text.Regex.Applicative ( RE, sym, (=~), (<|>), string, anySym, psym )
import Text.Regex.Applicative.Common (decimal)

type Parser a = RE Char a

sepBy :: Alternative f => f a -> f b -> f [a]
p `sepBy` sep = (:) <$> p <*> many (sep *> p)

data Part = Part { excellence, musicality, aerodynamism, shininess :: Int } deriving Show
type Label = String
data Verdict = Accept | Reject deriving (Show, Eq)
data Outcome = Final Verdict | Transition Label deriving Show
data Field = Excellence | Musicality | Aerodynamism | Shininess deriving (Show, Eq, Ord, Enum, Bounded)
data Op = Greater | Less deriving Show
data Condition = Condition Field Op Int deriving Show
data Step = Step Condition Outcome deriving Show
data Workflow = Workflow Label [Step] Outcome deriving Show
data Handler r = Handler { combine :: Condition -> r -> r -> r, final :: Verdict -> r }
type CompiledWorkflow = forall r. Handler r -> r

data Input a = Input a [Part] deriving Functor

type Inp = Input CompiledWorkflow

score :: Part -> Int
score (Part x m a s) = x + m + a + s

compileField :: Field -> Part -> Int
compileField Excellence = excellence
compileField Musicality = musicality
compileField Aerodynamism = aerodynamism
compileField Shininess = shininess

compileOp :: Op -> Int -> Int -> Bool
compileOp Greater = (>)
compileOp Less = (<)

compileCondition :: Condition -> (Part -> Verdict) -> (Part -> Verdict) -> (Part -> Verdict)
compileCondition (Condition field op threshold) met failed = run
  where run p | acceptable p = met p
              | otherwise = failed p
        acceptable p = get p ?= threshold
        get = compileField field
        (?=) = compileOp op


pipeline :: CompiledWorkflow -> Part -> Verdict
pipeline workflow = workflow (Handler compileCondition const)

part1 :: Inp -> Int
part1 (Input workflow parts) = sum . map score . filter ((== Accept) . pipeline workflow) $ parts

universe :: ConstraintSet
universe = M.fromList [(k, V2 1 4000) | k <- [minBound ..]]

type ConstraintSet = M.Map Field (V2 Int)
finalize :: Verdict -> [ConstraintSet]
finalize Reject = []
finalize Accept = [universe]

choose :: Condition -> [ConstraintSet] -> [ConstraintSet] -> [ConstraintSet]
choose (Condition field op threshold) yes no = (require field op threshold <$> yes) <> (forbid field op threshold <$> no)
  where require field op threshold = M.adjust f field
          where f = case op of 
                      Greater -> over _x (max (threshold + 1))
                      Less -> over _y (min (threshold - 1))
        forbid field op threshold = M.adjust f field
          where f = case op of 
                      Greater -> over _y (min threshold)
                      Less -> over _x (max threshold)

fieldOptions :: V2 Int -> Int
fieldOptions (V2 lo hi) = max 0 (hi - lo + 1)

countOptions :: ConstraintSet -> Int
countOptions = product . map fieldOptions . M.elems

part2 :: Inp -> Int
part2 (Input workflow _) = sum . map countOptions . workflow $ Handler choose finalize

attr :: Parser Int
attr = replicateM_ 3 anySym *> decimal

part :: Parser Part
part = Part <$> attr <*> attr <*> attr <*> (attr <* sym '}')

label :: Parser Label
label = some (psym isLower)

verdict :: Parser Verdict
verdict = (Accept <$ sym 'A') <|> (Reject <$ sym 'R')

outcome :: Parser Outcome
outcome = (Final <$> verdict) <|> (Transition <$> label)

field :: Parser Field
field = asum (zipWith go "xmas" [minBound ..])
  where go c f = f <$ sym c

condition :: Parser Condition
condition = Condition <$> field <*> ((Greater <$ sym '>') <|> (Less <$ sym '<')) <*> decimal

step :: Parser Step
step = Step <$> condition <* sym ':' <*> outcome

workflow :: Parser Workflow
workflow = Workflow <$> label <* sym '{' <*> (step `sepBy` sym ',') <*> (sym ',' *> outcome) <* sym '}'

compile :: [Workflow] -> CompiledWorkflow
compile flows = m M.! "in"
  where m = M.fromList . map compileWorkFlow $ flows
        compileWorkFlow :: Workflow -> (Label, CompiledWorkflow)
        compileWorkFlow (Workflow label steps fallback) =
          (label, foldr compileStep (compileOutcome fallback) steps)
        compileOutcome :: Outcome -> CompiledWorkflow
        compileOutcome (Final verdict) = flip final verdict
        compileOutcome (Transition label) = m M.! label
        compileStep :: Step -> CompiledWorkflow -> CompiledWorkflow
        compileStep (Step cond outcome) next k =
          combine k cond (compileOutcome outcome k) (next k)

solve :: String -> IO ()
solve input = putStrLn "--- Day 19  ---" >> print (part1 $ p input) >> print (part2 $ p input)
  where
    p = fmap compile . fromMaybe (error "Invalid input") . (=~ parse)
    parse = Input <$> some (workflow <* sym '\n') <* sym '\n' <*> some (part <* sym '\n')
