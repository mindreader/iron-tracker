{-# LANGUAGE OverloadedStrings, TemplateHaskell, ScopedTypeVariables #-}
module WorkoutPlan where


import Control.Monad (foldM)
import Control.Applicative
import Data.Monoid

import Data.Time

import Control.Lens


import qualified Data.Map as M
import qualified Data.Text as T
import Data.List as L

import Weight.Types
import qualified Weight.PlateOrder as PO
import qualified Weight.PlateCalc as PC
import Weight.Log
import Weight.Formulas

data WorkoutPlan = Plan [WorkoutStep] deriving Show

type PlateOrder = [PO.Plate]

data WorkoutStep =
  BarbellExercise {
    _sExercise   :: Exercise,
    _sReps       :: Reps,
    _sWeight     :: Weight,
    _sPlateOrder :: PlateOrder
  } |
  DumbbellExercise {
    _sExercise :: Exercise,
    _sReps     :: Reps,
    _sWeight   :: Weight
  } |
  BodyWeightExercise {
    _sExercise :: Exercise,
    _sReps     :: Reps
  } deriving Show

type HistFunc m = (Exercise -> m [(Day, (Reps, Proficiency))])
type SuggestionFunc = (CycleLength -> Day -> [(Day, (Reps, Proficiency))] -> TryThis)

makeLenses ''WorkoutPlan
makeLenses ''WorkoutStep


plan :: WeightState -> CycleLength -> IO WorkoutPlan
plan ws cycle = do
  currentWorkout <- L.sort <$> dbFilterCurrentWorkout (ws ^. exercises . to M.elems)
  plan' currentWorkout cycle (flip pastHistory 5) undefined undefined



plan' :: forall m. (Functor m, Monad m) => [Exercise] -> CycleLength -> HistFunc m -> SuggestionFunc -> m Day -> m WorkoutPlan
plan' exers wCycle histf suggest todayf = Plan . L.reverse <$> foldM nextStep [] exers
  where

    nextStep :: [WorkoutStep] -> -- Workout up to this point
                Exercise ->      -- Exercise for this step
                m [WorkoutStep]  -- Prepend next step to workout so far.

    nextStep sofarSteps exercise = do
      hist <- histf exercise
      today <- todayf
      let recentRepAverage = average . fmap (view $ _2 . _2 . pReps) . L.take 3 $ hist :: Reps

      return . reorderBarbells $ case exercise ^. eType of
        Dumbbell   ->
          -- Preliminary guestimate for dumbbells because I don't know how I want to do them yet.
          let attemptWeight = maximum . map (view $ _2 . _2 . pWeight) . L.take 3 $ hist :: Weight
          in DumbbellExercise exercise recentRepAverage attemptWeight : sofarSteps

        Bodyweight ->
          -- Attempted reps is the average rounded up of the last few (up to 3) workouts.
          BodyWeightExercise exercise recentRepAverage : sofarSteps

        Barbell    ->
          let
              -- 1. modify history into [didthis]
              histToDidThis (day, (told, (Pro r w))) = DidThis told r w day
              didThese = histToDidThis <$> hist

              -- 2. suggestNewRepWeight :: CycleLength -> Day -> [DidThis] -> TryThis
              -- based on what you did recently and the current day, try this
              (TryThis tr tw) = suggestNewRepWeight wCycle today didThese

              -- 3. modify trythis weight into plate order (suboptimal, we'll optimize later)
              plateOrder = plateCalc2Order $ case PC.plateCalc PC.Barbell tw of (PC.Plates p) -> p

          in (BarbellExercise exercise tr tw plateOrder:sofarSteps)

    reorderBarbells steps = undefined

average [] = 0
average xs = ceiling $ (fromIntegral $ sum' xs) / (fromIntegral $ length xs)
  where
    sum' = foldr1 (+)


prop_plan :: (Functor m, Monad m) => WeightState -> m WorkoutPlan
prop_plan _ = plan' testWorkout 12 historyGetter undefined undefined
  where
    testWorkout = [Exercise "bsquats" "barbell squats" 2 Barbell 1]
    historyGetter x = return []

-- TODO move to general purpose plate library for cleaner interface.
plateCalc2Order :: [PC.Plate] -> [PO.Plate]
plateCalc2Order (PC.P45  n :ps) = replicate (n `div` 2) PO.P45  <> plateCalc2Order ps
plateCalc2Order (PC.P25  n :ps) = replicate (n `div` 2) PO.P25  <> plateCalc2Order ps
plateCalc2Order (PC.P10  n :ps) = replicate (n `div` 2) PO.P10  <> plateCalc2Order ps
plateCalc2Order (PC.P5   n :ps) = replicate (n `div` 2) PO.P5   <> plateCalc2Order ps
plateCalc2Order (PC.P2p5 n :ps) = replicate (n `div` 2) PO.P2p5 <> plateCalc2Order ps
plateCalc2Order _ = []


