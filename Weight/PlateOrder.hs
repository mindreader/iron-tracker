{-# LANGUAGE FlexibleInstances #-}

module Weight.PlateOrder (
Plate(..),Lift,Workout,optimalPlateOrder, displayPlates
) where
-- module Main where

import qualified Data.List as L
import Control.Monad
import Data.Set as S
import Data.Function (on)
import Control.Monad (sequence)

data Plate = P45 | P25 | P10 | P5 | P2p5 deriving (Eq, Ord, Show)

type Lift = [Plate]
type Workout = [Lift]


squats   = [P45,P45,P5]
gmorning = [P45,P25,P10,P5]
shrugs   = [P45,P25,P10]
bench    = []
bentrow  = [P45,P10,P5]
overhead = [P10,P10,P2p5]
curls    = [P10,P5]
triceps  = [P10,P2p5]


main =  blah [] testWorkout
    where
      blah _ [] = []
      blah last stuff = do
        let this = head $ optimalPlateOrder last stuff
        this:blah this (tail stuff)

displayPlates :: Lift -> String
displayPlates [] = "just the bar"
displayPlates ps = dPlates ps
  where
    dPlates :: [Plate] -> String
    dPlates = concat . L.intersperse "," . L.map dPlate
    dPlate :: Plate -> String
    dPlate P45  = "45"
    dPlate P25  = "25"
    dPlate P10  = "10"
    dPlate P5   = "5"
    dPlate P2p5 = "2.5"



testWorkout :: Workout
testWorkout = [squats, gmorning, shrugs, bench, bentrow, overhead, curls, triceps]

complexity :: Workout -> Integer
complexity = product . L.map (fromIntegral . L.length)


pcost :: Plate -> Int
pcost P45  = 22
pcost P25  = 17
pcost P10  = 13
pcost P5   = 12
pcost P2p5 = 10


desireable :: Lift -> Bool
-- desireable (P45:P45:P45:P45:xs) = True  These are way out of my range, no point in checking
-- desireable (P45:P45:P45:P45:P45:xs) = True
-- desireable (_:_:P45:P45:_) = False
-- desireable (_:_:_:P45:P45:_) = False
desireable _ = True


-- Prepend the current state of the bar to all possible workouts that spring from it.
-- Limit to 4 workouts to ease computation.
-- WARNING - if there is a bodyweight exercise in middle of workout, that will jack up this algorithm
--   (it will assume you intend to take all weight off bar for that exercise).
optimalPlateOrder :: Lift -> Workout -> Workout
optimalPlateOrder initialLift = minWorkout . (L.map (initialLift :)) . L.map (L.filter desireable) . allPossibleWorkouts . take 4
  where
    minWorkout :: [Workout] -> Workout
    minWorkout xs = tail . snd . L.minimumBy (compare `on` fst) . reverse . zip (L.map wCost xs) $ xs

-- Every possible combination of plate orderings given an initial set of plates per exercise.
allPossibleWorkouts :: Workout -> [Workout]
allPossibleWorkouts = sequence . L.map combinations
  where
    combinations :: Ord a => [a] -> [[a]]
    combinations = L.nub . L.permutations

-- Cost of switching plates in a workout from beginning to end.
wCost :: Workout -> Int
wCost w = case w of
  []       -> 0
  (x:y:xs) -> lCost x y + wCost (y:xs)
  _        -> 0
  where
    lCost :: Lift -> Lift -> Int
    lCost [] [] = 0
    lCost [] y = sum' (L.map pcost y)
    lCost x [] = sum' (L.map pcost x)
    lCost x'@(x:xs) y'@(y:ys) =  if x == y
      then lCost xs ys
      else sum' (L.map pcost x') + sum' (L.map pcost y')

    sum' = L.foldl' (+) 0
