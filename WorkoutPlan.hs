{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}
module WorkoutPlan (workoutPlan, WorkoutPlan(..), WorkoutStep(..)) where

import BasicPrelude

import Control.Monad (foldM)

import Data.Time

import Control.Lens


import qualified Data.Map as M

import Weight.Types
import qualified Weight.PlateOrder as PO
import qualified Weight.PlateCalc as PC
import Weight.Log
import Weight.Formulas

type HistFunc m = (Exercise -> m [(Day, (Reps, Proficiency))])
type SuggestionFunc = ([DidThis] -> TryThis)


workoutPlan :: WeightState -> CycleLength -> IO WorkoutPlan
workoutPlan ws cycle = do
  currentWorkout <- sort <$> dbFilterCurrentWorkout (ws ^. exercises . to M.elems)
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  plan' currentWorkout cycle (flip pastHistory 5) (suggestNewRepWeight cycle today)



plan' :: forall m. (Functor m, Monad m) => [Exercise] -> CycleLength -> HistFunc m -> SuggestionFunc -> m WorkoutPlan
plan' exers wCycle histf suggestf = Plan . reverse <$> foldM nextStep [] exers
  where

    nextStep :: [WorkoutStep] -> -- Workout up to this point
                Exercise ->      -- Exercise for this step
                m [WorkoutStep]  -- Prepend next step to workout so far.

    nextStep sofarSteps exercise = do
      hist <- histf exercise
      let recentRepAverage = average . fmap (view $ _2 . _2 . pReps) . take 3 $ hist :: Reps

      return . reorderBarbells $ case exercise ^. eType of
        Dumbbell ->
          -- Preliminary guestimate for dumbbells because I don't know how I want to do them yet.
          let attemptWeight = maximum . fmap (view $ _2 . _2 . pWeight) . take 3 $ hist :: Weight
          in DumbbellExercise exercise recentRepAverage attemptWeight : sofarSteps

        Bodyweight ->
          -- Attempted reps is the average rounded up of the last few (up to 3) workouts.
          BodyWeightExercise exercise recentRepAverage : sofarSteps

        Barbell ->
          let
              -- 1. modify history into [didthis]
              hist2DidThis (day, (told, (Pro r w))) = DidThis told r w day
              didThese = hist2DidThis <$> hist

              -- 2. suggestNewRepWeight :: CycleLength -> Day -> [DidThis] -> TryThis
              -- based on what you did recently and the current day, try this
              (TryThis tr tw) = suggestf didThese

              -- 3. modify trythis weight into plate order (suboptimal, we'll optimize later)
              plateOrder = plateCalc2Order . PC.getPlates . PC.plateCalc PC.Barbell $ tw

          in (BarbellExercise exercise tr tw plateOrder:sofarSteps)

    -- 4. here's where we optimize the plate orders.
    reorderBarbells [] = []
    reorderBarbells steps = reorderBarbells' [] steps
      where
        reorderBarbells' :: [PO.Plate] -> [WorkoutStep] -> [WorkoutStep]
        reorderBarbells' _ [] = []
        reorderBarbells' prev (step@(BarbellExercise e r w po):xs) =
          let
            curPlateOrder = PO.optimalPlateOrder prev (fmap (\(BarbellExercise _ _ _ po) -> po) . filter isBarbell $ (step:xs))
            current = BarbellExercise e r w curPlateOrder
          in current : reorderBarbells' curPlateOrder xs

        reorderBarbells' prev (step:xs) = step:reorderBarbells' prev xs

        isBarbell (BarbellExercise {}) = True
        iSBarbell _ = False

average [] = 0
average xs = ceiling $ (fromIntegral $ sum' xs) / (fromIntegral $ length xs)
  where
    sum' = foldr1 (+)

{-
test_workoutPlanEmpty :: Test
test_workoutPlanEmpty =
  let today = read "2013-04-23"
  in TestCase $ assertEqual "workoutPlanEmpty" (plan' [] 12 (return []) (suggestNewRepWeight 12 today)) (return $ Plan [])

prop_workoutPlan :: (Functor m, Monad m) => WeightState -> m WorkoutPlan
prop_workoutPlan _ = plan' testWorkout 12 historyGetter (suggestNewRepWeight 12 undefined)
  where
    testWorkout = [Exercise "bsquats" "barbell squats" 2 Barbell 1]
    historyGetter x = return []
-}

-- TODO move to general purpose plate library for cleaner interface.
plateCalc2Order :: [PC.Plate] -> [PO.Plate]
plateCalc2Order (PC.P45  n :ps) = replicate (n `div` 2) PO.P45  <> plateCalc2Order ps
plateCalc2Order (PC.P25  n :ps) = replicate (n `div` 2) PO.P25  <> plateCalc2Order ps
plateCalc2Order (PC.P10  n :ps) = replicate (n `div` 2) PO.P10  <> plateCalc2Order ps
plateCalc2Order (PC.P5   n :ps) = replicate (n `div` 2) PO.P5   <> plateCalc2Order ps
plateCalc2Order (PC.P2p5 n :ps) = replicate (n `div` 2) PO.P2p5 <> plateCalc2Order ps
plateCalc2Order _ = []


