{-# LANGUAGE OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Weight(
  runWeightRoutine
) where

import Control.Lens
import Data.Time (Day)

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L

import Data.Default
import Safe (headMay)


import Control.Monad.State
import Control.Applicative

import Weight.Formulas
import Weight.Config
import Weight.Types
import Weight.Log
import qualified Weight.PlateCalc as PC
import Weight.PlateOrder as PO
import WorkoutPlan

import Menu
import IO

import Control.Exception (bracket_)

data WeightMenuCommand = MWWorkoutMode | MWWorkoutStatus | MWAdjustWorkoutReps | MWUpdate | MWInclude | MWDisInclude deriving (Eq, Ord)

newtype App a = App (StateT AppState IO a)
  deriving (Monad, MonadState AppState, MonadIO, Functor, Applicative)

data AppState = AS {
  _weightState :: WeightState,
  _wCycle :: Integer
}

makeLenses ''AppState

runWeightRoutine :: IO ()
runWeightRoutine = runApp mainLoop

runApp :: App a -> IO a
runApp (App s) = do
  ws <- loadWeightConfig
  evalStateT s (AS ws 12)


mainLoop :: App ()
mainLoop = do
  command <- inputMenu (def { quitOption = True }) "Weight Menu" menuCrud

  case command of
    MenuError -> mainLoop
    MenuQuit -> return ()
    MenuInput command' -> do
      case command' of
        MWWorkoutMode       -> workoutMode
        MWWorkoutStatus     -> printWorkout id
        MWAdjustWorkoutReps -> adjustWorkoutByReps
        MWUpdate            -> updateSingleExercise
        MWInclude           -> addExerciseToWorkout
        MWDisInclude        -> remExerciseFromWorkout
      continue

  where
    continue = pressAnyKey >> mainLoop
    menuCrud = [
      (MWWorkoutMode,       "Perform a Workout"),
      (MWAdjustWorkoutReps, "Print workout with new rep count"),
      (MWWorkoutStatus,     "Current workout proficiencies"::T.Text),
      (MWUpdate,            "Update an exercise in current workout"),
      (MWInclude,           "Add exercise to workout"),
      (MWDisInclude,        "Remove exercise from workout")]


-- !On `Day` you were told to do `Reps` but you were able to do (Proficiency Reps Weight).
type History = [(Day, (Reps, Proficiency))]

workoutMode :: App ()
workoutMode = do
  ws <- use weightState
  wCycle <- use wCycle
  (Plan steps) <- liftIO $ workoutPlan ws wCycle
  forM_ steps $ \step -> do
    beforeStep step
    workoutStep step
    afterStep step
  liftIO $ printf "Workout complete.\n"
  where
    workoutStep (BarbellExercise exer reps weight plates) = do
      liftIO $ printf "You must do %s.\n" (formatRepsWeight reps weight (Just $ PO.displayPlates plates))
    workoutStep (DumbbellExercise exer reps weight) = do
      liftIO $ printf "You must do %s.\n" (formatRepsWeight reps weight Nothing)
    workoutStep (BodyWeightExercise exer reps) = do
      liftIO $ printf "You must do %d reps.\n" reps

    beforeStep step = liftIO (pastHistory (_sExercise step) 5) >>= printHistory
    afterStep step = inputReps >>= (\reps -> logLift (_sExercise step) (_sReps step, Pro reps (_sWeight step)))

    formatRepsWeight :: Reps -> Weight -> (Maybe String) -> String
    -- TODO this really should suggest the last weight we did.  If we did it over two weeks ago, it doesn't have the info at this point in code.
    formatRepsWeight reps 0.0 _ = printf "%d" reps
    formatRepsWeight reps weight (Just plates) = printf "%d@%s (%s)" reps (rTrimZeros $ show $ (fromRational weight :: Double)) plates
    formatRepsWeight reps weight Nothing = printf "%d@%s (%s)" reps (rTrimZeros $ show $ (fromRational weight :: Double)) (PC.displayPlateCalc PC.Barbell weight)

    printHistory :: History -> App ()
    printHistory history = do
      mapM_ printHistory' (L.reverse history)
      where
        printHistory' (day, (attempted,pro)) = liftIO $ printf " %s : tried %d did %s\n" (show day) attempted (formatRepsWeight (pro ^. pReps) (pro ^. pWeight) Nothing)


inputReps :: App Reps
inputReps = liftIO $ prompt "New reps:"
inputWeight :: App Weight
inputWeight = liftIO $ prompt "New weight:"

inputProficiency :: App Proficiency
inputProficiency = Pro <$> inputReps <*> inputWeight


exerciseList = use (weightState . exercises . to M.elems)
currentWorkout = fmap L.sort $ exerciseList >>= dbFilterCurrentWorkout

printWorkout :: (Proficiency -> Proficiency) -> App ()
printWorkout adjuster = do
  exers <- currentWorkout
  let maxExerNameLen = T.length $ L.maximumBy (\x y -> compare (T.length (x ^. eName)) (T.length (y ^. eName))) exers ^. eName
  when (null exers) $ liftIO $ printf "You do not have any exercises set up in your workout.\n"
  mapM exerWithProf exers >>= mapM_ (liftIO . putStrLn . formatLine maxExerNameLen)
  where
    exerWithProf :: Exercise -> App (Either Exercise (Day, Proficiency, Exercise))
    exerWithProf exer = do
      last <- allHistory exer 1
      case headMay last of
        Nothing          -> return . Left $ exer
        Just (day, (_, prof)) -> return . Right $ (day, adjuster prof, exer)
        
    formatLine namelen (Left exercise) = printf ("%" ++ show namelen ++ "s : (never done)") (exercise ^. eName)
    formatLine namelen (Right (date, Pro reps 0.0, exercise)) = printf ("%" ++ show namelen ++ "s : %d") (exercise ^. eName) reps
    formatLine namelen (Right (date, Pro reps weight, exercise)) = printf ("%" ++ show namelen ++ "s : %d@%.3s (%s)") (exercise ^. eName) reps (rTrimZeros $ show $ fromRational weight) (PC.displayPlateCalc PC.Barbell weight)

rTrimZeros :: String -> String
rTrimZeros = L.reverse . L.dropWhile (=='.') . L.dropWhile (== '0') . L.reverse

adjustWorkoutByReps :: App ()
adjustWorkoutByReps = do
  newreps <- liftIO $ prompt "Reps you want to do:"
  printWorkout $ adjustProfByReps newreps


updateSingleExercise :: App ()
updateSingleExercise = do
  exercises <- exerciseList
  mexer <- liftIO $ inputMenu def "Pick Exercise To Update" exercises
  case mexer of
    MenuError -> liftIO $ printf "You need to input some exercises to your config file."
    MenuInput exer -> inputProficiency >>= (\pro -> logLift exer (pro ^. pReps, pro))
    MenuQuit -> return ()
  return ()

addExerciseToWorkout :: App ()
addExerciseToWorkout = do
  exercises <- exerciseList
  workout <- currentWorkout
  let exercisesnotinworkout = exercises L.\\ workout
  mexer <- liftIO $ inputMenu def "Exercises Not In Workout" exercisesnotinworkout
  case mexer of
    MenuError -> liftIO $ printf "You already have every known exercise in your workout.\n"
    MenuInput exer -> dbAddExerciseToWorkout exer
    MenuQuit -> return ()
 
remExerciseFromWorkout :: App ()
remExerciseFromWorkout = do
  workout <- currentWorkout
  mexer <- liftIO $ inputMenu def "Exercises In Workout" workout
  case mexer of
    MenuError -> liftIO $ printf "You don't have any exercises in your workout.\n"
    MenuInput exer -> dbRemExerciseFromWorkout exer
    MenuQuit -> return ()


--weightRoutine :: App ()
--weightRoutine = do
--  x <- liftHistory "squats" 3
--  return ()
--  print $  firstOf (exercises . at "bdeads" . traverse . eName) x
--  logLift $ Proficiency "squats" 185 5
--  return ()

