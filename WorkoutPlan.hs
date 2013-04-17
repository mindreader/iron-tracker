{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module WorkoutPlan where


import Data.Time

import Control.Lens


import qualified Data.Map as M
import qualified Data.Text as T
import Data.List as L

import Weight.Types
import qualified Weight.PlateOrder as PO
import Weight.Log
import Weight.Formulas

data WorkoutPlan = Plan [WorkoutStep] deriving Show

data WorkoutStep =
  BarbellExercise {
    _sExercise :: Exercise,
    _sLift :: PO.Lift
  } |
  DumbbellExercise {
    _sExercise :: Exercise,
    _sLift :: PO.Lift
  } |
  BodyWeightExercise {
    _sExercise :: Exercise
  } deriving Show

type HistFunc m = (Exercise -> m [(Day, (Int, Proficiency))])
type SuggestionFunc = (CycleLength -> Day -> [(Day, (Int, Proficiency))] -> TryThis)

makeLenses ''WorkoutPlan
makeLenses ''WorkoutStep


plan :: WeightState -> CycleLength -> IO WorkoutPlan
plan ws cycle = do
  currentWorkout <- fmap L.sort $ dbFilterCurrentWorkout $  ws ^. exercises . to M.elems
  plan' currentWorkout cycle (flip pastHistory 5) undefined


prop_plan :: Monad m => WeightState -> m WorkoutPlan
prop_plan _ = plan' testWorkout 12 historyGetter undefined
  where
    testWorkout = [Exercise "bsquats" "barbell squats" 2 Dumbbell 1]
    historyGetter x = return []


plan' :: Monad m => [Exercise] -> CycleLength -> HistFunc m -> SuggestionFunc -> m WorkoutPlan
plan' exers wCycle histf suggest = do
  undefined
