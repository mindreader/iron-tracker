-- module Weight.PlateOrder where
 module Main where

import qualified Data.List as L
import Control.Monad
import Data.Set as S
import Data.Function (on)
import Control.Monad (sequence)

data Plate = P45 | P25 | P10 | P5 | P2p5 deriving (Eq, Ord, Show)

type Lift = [Plate]
type Workout = [Lift]

squats   = [P45,P45,P10,P2p5]
gmorning = [P45,P25,P10]
shrugs   = [P45,P25,P10,P5,P2p5]
bench    = [P45,P10,P5,P2p5]
bentrow  = [P45,P5]
overhead = [P25,P5,P2p5]
curls    = [P10,P5]
triceps  = [P10,P5,P2p5]

main = print $ optimalPlateOrder $ testWorkout

testWorkout :: Workout
testWorkout = [squats, gmorning, shrugs, bench, bentrow, overhead,curls,triceps]

class Cost a where
  cost :: a -> Int

instance Cost Plate where
  cost P45  = 22
  cost P25  = 17
  cost P10  = 13
  cost P5   = 12
  cost P2p5 = 10



desireable :: Lift -> Bool
-- desireable (P45:P45:P45:P45:xs) = True  These are way out of my range, no point in checking
-- desireable (P45:P45:P45:P45:P45:xs) = True
desireable (_:_:P45:P45:_) = False
desireable (_:_:_:P45:P45:_) = False
desireable _ = True

optimalPlateOrder :: Workout -> Maybe Workout
optimalPlateOrder = minWorkout . allPossibleWorkouts
  where
    minWorkout :: [Workout] -> Maybe Workout
    minWorkout [] = Nothing
    minWorkout xs = Just . snd . L.minimumBy (compare `on` fst) . zip (L.map wDistance xs) $ xs

allPossibleWorkouts :: Workout -> [Workout]
allPossibleWorkouts = sequence . L.map (L.filter desireable) . L.map combinations
  where
    combinations :: Ord a => [a] -> [[a]]
    combinations = L.nub . L.permutations


wDistance :: Workout -> Int
wDistance w = case w of
  []       -> 0
  (x:y:xs) -> lDistance x y + wDistance (y:xs)
  _        -> 0
  where
    lDistance :: Lift -> Lift -> Int
    lDistance [] [] = 0
    lDistance [] y = sum' (L.map cost y)
    lDistance x [] = sum' (L.map cost x)
    lDistance x'@(x:xs) y'@(y:ys) =  if x == y
      then lDistance xs ys
      else sum' (L.map cost x') + sum' (L.map cost y')

    sum' = L.foldl' (+) 0
