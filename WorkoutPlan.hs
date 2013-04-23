{-# LANGUAGE OverloadedStrings, TemplateHaskell, ScopedTypeVariables #-}
module WorkoutPlan where


import Control.Monad (foldM)
import Control.Applicative

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
    _sPlateOrder :: PO.Lift
  } |
  DumbbellExercise {
    _sExercise :: Exercise,
    _sReps :: Reps,
    _sWeight :: Weight
  } |
  BodyWeightExercise {
    _sExercise :: Exercise,
    _sReps :: Reps
  } deriving Show

type HistFunc m = (Exercise -> m [(Day, (Int, Proficiency))])
type SuggestionFunc = (CycleLength -> Day -> [(Day, (Int, Proficiency))] -> TryThis)

makeLenses ''WorkoutPlan
makeLenses ''WorkoutStep


plan :: WeightState -> CycleLength -> IO WorkoutPlan
plan ws cycle = do
  currentWorkout <- L.sort <$> dbFilterCurrentWorkout (ws ^. exercises . to M.elems)
  plan' currentWorkout cycle (flip pastHistory 5) undefined



plan' :: forall m. (Functor m, Monad m) => [Exercise] -> CycleLength -> HistFunc m -> SuggestionFunc -> m WorkoutPlan
plan' exers wCycle histf suggest = Plan . L.reverse <$> foldM nextStep [] exers
  where

    nextStep :: [WorkoutStep] -> -- Workout up to this point
                Exercise ->      -- Exercise for this step
                m [WorkoutStep]  -- Prepend next step to workout so far.

    nextStep sofarSteps exercise = do
      hist <- histf exercise
      let recentRepAverage = average . fmap (view $ _2 . _2 . pReps) . L.take 3 $ hist :: Reps

      return $ case exercise ^. eType of
        Barbell    -> (undefined:sofarSteps)

        Dumbbell   ->

          -- Preliminary guestimate for dumbbells because I don't know how I want to do them yet.
          let attemptWeight = maximum . map (view $ _2 . _2 . pWeight) . L.take 3 $ hist :: Weight
          in DumbbellExercise exercise recentRepAverage attemptWeight : sofarSteps

        Bodyweight ->
          -- Attempted reps is the average rounded up of the last few (up to 3) workouts.
          BodyWeightExercise exercise recentRepAverage : sofarSteps

average [] = 0
average xs = ceiling $ (fromIntegral $ sum' xs) / (fromIntegral $ length xs)
  where
    sum' = foldr1 (+)


prop_plan :: (Functor m, Monad m) => WeightState -> m WorkoutPlan
prop_plan _ = plan' testWorkout 12 historyGetter undefined
  where
    testWorkout = [Exercise "bsquats" "barbell squats" 2 Barbell 1]
    historyGetter x = return []


