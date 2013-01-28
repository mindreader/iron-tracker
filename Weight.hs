{-# LANGUAGE OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Weight(
  runWeightRoutine
) where

import Control.Lens
import Data.Time (localDay,zonedTimeToLocalTime,getZonedTime,Day)

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L

import Data.Default
import Safe


import Control.Monad.State

import Weight.Formulas
import Weight.Config
import Weight.Types
import Weight.Log
import Weight.PlateCalc

import Menu
import IO

data WeightMenuCommand = MWWorkoutMode | MWWorkoutStatus | MWAdjustWorkoutReps | MWUpdate | MWInclude | MWDisInclude deriving (Eq, Ord)

newtype App a = App (StateT AppState IO a)
  deriving (Monad, MonadState AppState, MonadIO, Functor)

data AppState = AS {
  _weightState :: WeightState,
  _today :: Day
}

makeLenses ''AppState

runWeightRoutine :: IO ()
runWeightRoutine = runApp mainLoop

runApp :: App a -> IO a
runApp (App s) = do
  today <- fmap (localDay . zonedTimeToLocalTime) getZonedTime
  ws <- loadWeightConfig
  evalStateT s (AS ws today)


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




workoutMode :: App ()
workoutMode = do
  workout <- currentWorkout
  today <- fmap (\x -> x ^. today) get
  mapM_ (doExercise today) workout
  liftIO $ printf "Workout complete.\n"
  where
    doExercise :: Day ->  Exercise -> App ()
    doExercise today exer = do
      history <- liftHistory exer 4
      let (TryThis tr tw) = suggestNewRepWeight today $ map (\(date,(attemptedReps,Pro r w)) -> DidThis attemptedReps (fromIntegral r) w date) history
      liftIO $ printf "\n%s\n" (exer ^. eName)
      printHistory history
      liftIO $ printf "You must do %s.\n" (formatRepsWeight tr tw)
      change <- liftIO $ prompt "Any change? (y/n)"
      if (change == ("y"::String))
        then inputProficiency exer >>= (\pro -> logLift exer (tr, pro))
        else case headMay history of
               Nothing -> return ()
               Just (_,prof) -> logLift exer prof

    printHistory :: [(Day, (Int, Proficiency))] -> App ()
    printHistory history = do
      mapM_ printHistory' (L.reverse history)
      where
        printHistory' (day, (_,pro)) = liftIO $ printf " %s : %s\n" (show day) (formatRepsWeight (pro ^. pReps) (pro ^. pWeight))

    formatRepsWeight :: Reps -> Weight -> String
    -- TODO this really should suggest the last weight we did.  If we did it over two weeks ago, it doesn't have the info at this point in code.
    formatRepsWeight reps 0.0 = printf "%d at whatever you think you can do" reps
    formatRepsWeight reps weight = printf "%d@%s (%s)" reps (rTrimZeros $ show $ (fromRational weight :: Double)) (displayPlateCalc $ weight)


inputProficiency :: Exercise -> App Proficiency
inputProficiency exer = do
      newreps   <- liftIO $ prompt "New reps:"
      newweight <- liftIO $ prompt "New weight (0 for bodyweight):"
      return $ Pro newreps newweight


exerciseList = fmap (fmap snd . M.toList. (\x -> x ^. weightState ^. exercises)) get
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
      last <- liftHistory exer 1
      case headMay last of
        Nothing          -> return . Left $ exer
        Just (day, (_, prof)) -> return . Right $ (day, adjuster prof, exer)
        
    formatLine namelen (Left exercise) = printf ("%" ++ show namelen ++ "s : (never done)") (exercise ^. eName)
    formatLine namelen (Right (date, Pro reps 0.0, exercise)) = printf ("%" ++ show namelen ++ "s : %d") (exercise ^. eName) reps
    formatLine namelen (Right (date, Pro reps weight, exercise)) = printf ("%" ++ show namelen ++ "s : %d@%.3s (%s)") (exercise ^. eName) reps (rTrimZeros $ show $ fromRational weight) (displayPlateCalc $ weight)

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
    MenuInput exer -> inputProficiency exer >>= (\pro -> logLift exer (pro ^. pReps, pro))
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

