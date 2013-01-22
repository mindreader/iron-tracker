module Weight.PlateOrder where

import qualified Data.List as L
import Control.Monad
import Data.Set as S
import Data.Function (on)

data Plate = P45 | P25 | P10 | P5 | P2p5 deriving (Eq, Ord, Show)

type Lift = [Plate]
type Workout = [Lift]

lifta, liftb, liftc :: Lift
lifta = [P45,P45,P25,P5] --squats
liftb = [P45,P25,P10]    --bench
liftc = [P25,P5,P10]    --bent over row

testWorkout :: Workout
testWorkout = [lifta, liftb, liftc]

  


optimalPlateOrder :: Workout -> Workout
optimalPlateOrder = minWorkout . allPossibleWorkouts
  where
    minWorkout :: [Workout] -> Workout
    minWorkout xs = snd . L.minimumBy (compare `on` fst) . zip (L.map wDistance xs) $ xs

allPossibleWorkouts :: Workout -> [Workout]
allPossibleWorkouts = undefined . L.map combinations
  where
    combinations :: Ord a => [a] -> [[a]]
    combinations = S.toList . S.fromList . L.permutations


wDistance :: Workout -> Int
wDistance w = case w of
  []       -> 0
  (x:y:xs) -> lDistance x y + wDistance (y:xs)
  _        -> 0
  where
    lDistance :: Lift -> Lift -> Int
    lDistance [] [] = 0
    lDistance [] y = length y
    lDistance x [] = length x
    lDistance x'@(x:xs) y'@(y:ys) = if x == y
      then lDistance xs ys
      else length x' + length x'


