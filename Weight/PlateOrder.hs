{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Weight.PlateOrder (
Plate(..),PlateOrder,Workout,optimalPlateOrder, displayPlates
) where

import BasicPrelude

import Weight.PlateOrder.Types

type Workout = [PlateOrder]

squats   = [P45,P45,P5]
gmorning = [P45,P25,P10,P5]
shrugs   = [P45,P25,P10]
bench    = []
bentrow  = [P45,P10,P5]
overhead = [P10,P10,P2p5]
curls    = [P10,P5]
triceps  = [P10,P2p5]

{-
main =  blah [] testWorkout
    where
      blah _ [] = []
      blah last stuff = do
        let this = head $ optimalPlateOrder last stuff
        this:blah this (tail stuff)
-}

displayPlates :: PlateOrder -> Text
displayPlates [] = "just the bar"
displayPlates ps = dPlates ps
  where
    dPlates :: [Plate] -> Text
    dPlates = mconcat . intersperse "," . fmap dPlate
    dPlate :: Plate -> Text
    dPlate P45  = "45"
    dPlate P25  = "25"
    dPlate P10  = "10"
    dPlate P5   = "5"
    dPlate P2p5 = "2.5"



testWorkout :: Workout
testWorkout = [squats, gmorning, shrugs, bench, bentrow, overhead, curls, triceps]

complexity :: Workout -> Integer
complexity = product . fmap (fromIntegral . length)


pcost :: Plate -> Int
pcost P45  = 22
pcost P25  = 17
pcost P10  = 13
pcost P5   = 12
pcost P2p5 = 10


desireable :: PlateOrder -> Bool
-- desireable (P45:P45:P45:P45:xs) = True  These are way out of my range, no point in checking
-- desireable (P45:P45:P45:P45:P45:xs) = True
-- desireable (_:_:P45:P45:_) = False
-- desireable (_:_:_:P45:P45:_) = False
desireable _ = True


-- Prepend the current state of the bar to all possible workouts that spring from it.
-- Limit to 4 workouts to ease computation.
-- WARNING - if there is a bodyweight exercise in middle of workout, that will jack up this algorithm
--   (it will assume you intend to take all weight off bar for that exercise).
optimalPlateOrder :: PlateOrder -> Workout -> PlateOrder
optimalPlateOrder initialLift = head . minWorkout . (fmap (initialLift :)) . fmap (filter desireable) . allPossibleWorkouts . take 4
  where
    minWorkout :: [Workout] -> Workout
    minWorkout xs = tail . snd . minimumBy (compare `on` fst) . reverse . zip (fmap wCost xs) $ xs

-- Every possible combination of plate orderings given an initial set of plates per exercise.
allPossibleWorkouts :: Workout -> [Workout]
allPossibleWorkouts = sequence . fmap combinations
  where
    combinations :: Ord a => [a] -> [[a]]
    combinations = nub . permutations

-- Cost of switching plates in a workout from beginning to end.
wCost :: Workout -> Int
wCost w = case w of
  []       -> 0
  (x:y:xs) -> lCost x y + wCost (y:xs)
  _        -> 0
  where
    lCost :: PlateOrder -> PlateOrder -> Int
    lCost [] [] = 0
    lCost [] y = sum' (fmap pcost y)
    lCost x [] = sum' (fmap pcost x)
    lCost x'@(x:xs) y'@(y:ys) =  if x == y
      then lCost xs ys
      else sum' (fmap pcost x') + sum' (fmap pcost y')

    sum' = foldl' (+) 0
