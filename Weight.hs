{-# LANGUAGE OverloadedStrings #-}

module Weight(runWeightRoutine) where

import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad (when)
import qualified Data.Text as T
import Menu
import Data.List ((\\))
import Data.Default (def)
import Data.Maybe (isJust)

import PlateCalc

import IO
import FitState

import System.Console.Haskeline (MonadException, InputT, runInputT, defaultSettings)
import Data.Time (Day)


data WeightMenuCommand = MWWorkoutMode | MWWorkoutStatus | MWAdjustWorkoutReps | MWUpdate | MWInclude | MWDisInclude | MWAdd | MWRemove deriving (Eq, Ord)

runWeightRoutine :: MonadException m => m ()
runWeightRoutine = runInputT defaultSettings (runFitStateT mainLoop)


mainLoop :: (MonadException m) => FitStateT (InputT m) ()
mainLoop = do
  command <- liftIO $ inputMenu (def { quitOption = True }) "Weight Menu" menuCrud

  case command of
    MenuError -> mainLoop
    MenuQuit -> return ()
    MenuInput command' -> do
      case command' of
        MWWorkoutMode       -> workoutMode
        MWWorkoutStatus     -> printWorkout id
        MWAdjustWorkoutReps -> adjustWorkoutByReps
        MWInclude           -> addExerciseToWorkout
        MWDisInclude        -> removeExerciseFromWorkout
        MWUpdate            -> updateExercise
        MWAdd               -> addNewExercise
        MWRemove            -> removeOldExercise
      continue

  where
    continue = pressAnyKey >> mainLoop
    menuCrud = [
      (MWWorkoutMode,       "Perform a Workout"),
      (MWAdjustWorkoutReps, "Print workout with new rep count"),
      (MWWorkoutStatus,     "Current workout proficiencies"::T.Text),
      (MWUpdate,            "Update an exercise in current workout"),
      (MWInclude,           "Add exercise to workout"),
      (MWDisInclude,        "Remove exercise from workout"),
      (MWAdd,               "Add New exercise"),
      (MWRemove,            "Remove exercise")]


printWorkout :: (MonadIO m) => (Proficiency -> Proficiency) -> FitStateT m ()
printWorkout f = do
  exers <- exercisesWithInfo currentWorkoutList
  when (null exers) $ liftIO $ printf "You do not have any exercises set up in your workout.\n"
  liftIO $ printTable . map formatExer . fmap (fmap (fmap (fmap f))) $ exers
  where
    formatExer (Exercise label, (date, prof)) = [T.unpack label, maybe "(none)" formatProficiency prof, maybe "" show date]
    formatProf Nothing = "(never done)"
    formatProf (Just (Proficiency 0 reps)) = show reps
    formatProf (Just (Proficiency weight reps)) = printf "%d@%d (%s)" reps weight (displayPlateCalc weight)


exercisesWithInfo :: Monad m => FitStateT m [Exercise] -> FitStateT m [(Exercise, (Maybe Day, Maybe Proficiency))]
exercisesWithInfo f = f >>= mapM addInfo
  where

    addInfo exer@(Exercise label) = do
      prof <- getProficiency label
      lastworkout <- getLastWorkout label
      return (exer, (lastworkout, prof))


formatProficiency :: Proficiency -> String
formatProficiency (Proficiency 0 reps)      = printf "%d" reps
formatProficiency (Proficiency weight reps) = printf "%s (%s)" (pad 6 $ printf "%d@%d" reps weight :: String) (displayPlateCalc weight)

workoutMode :: MonadException m => FitStateT (InputT m) ()
workoutMode = do
  newreps <- lift $ prompt "Reps you are aiming for:"
  let
    workoutMode' exers = do
      mexer <- liftIO $ inputMenu (def { quitOption = True }) "Select Next Exercise" exers
      case mexer of
        MenuError -> liftIO $ printf "You are finished with your workout.\n"
        MenuQuit -> return ()
        MenuInput exer -> do
          mprof <- getProficiency exer
          let newmprof = fmap (epleyize newreps) mprof
          mdate <- getLastWorkout exer
          liftIO $ printf "For %s you were last able to do %s on %s.\n" exer (maybe "*never done before*" formatProficiency mprof) (maybe "" show mdate)
          when (isJust newmprof) $ liftIO $ printf "You must do %s.\n" (maybe "" formatProficiency newmprof)
          change <- lift $ prompt "Any change? (y/n)"
          when (change == ("y"::String)) $ do
            updateExerciseProcedure exer
          setLastWorkout exer
          pressAnyKey
          workoutMode' (filter (\(Exercise label) -> label /= exer) exers)

--  profs <- fmap (sortBy (compare `on` snd . snd) . fmap (fmap (fmap (fmap $ epleyize newreps)))) $ exercisesWithInfo currentWorkoutList
  currentWorkoutList >>= workoutMode'


adjustWorkoutByReps :: (MonadException m) => FitStateT (InputT m) ()
adjustWorkoutByReps = do
  newreps <- lift $ prompt "Reps you want to do:"
  printWorkout $ epleyize newreps

epleyize :: Int -> Proficiency -> Proficiency
epleyize newreps (Proficiency weight reps) = Proficiency (epley reps weight newreps) newreps
  where
    --This one estimates suprisingly low on low rep ranges.
    --oconnor :: Int -> Int -> Int -> Int
    --oconnor r w r' = round $ (fromIntegral w) * (1+(0.025*(fromIntegral r))) / (1+(0.025 * (fromIntegral r')))

    --This one is almost dead on for what I want.
    epley :: Int -> Int -> Int -> Int
    epley r w r' = round $ ((fromIntegral w * fromIntegral r / 30) + fromIntegral w) * (30 / (fromIntegral r'+30))



 
updateExercise :: (MonadException m) => FitStateT (InputT m) ()
updateExercise = do
  workout <- currentWorkoutList

  mexer <- liftIO $ inputMenu def "Update Exercise" workout
  case mexer of
    MenuError -> liftIO $ printf "You don't have any exercises set up for your workout.\n"
    MenuInput exer -> updateExerciseProcedure exer
    MenuQuit -> return ()


updateExerciseProcedure :: (MonadException m) => T.Text -> FitStateT (InputT m) ()
updateExerciseProcedure exer = do
      mprof <- getProficiency exer
      case mprof of
        Nothing -> liftIO $ printf "You have never done this exercise.\n"
        Just (Proficiency weight reps)-> liftIO $ printf "You can currently do %d reps at %d pounds.\n" reps weight
      newreps   <- lift $ prompt "New reps:"
      newweight <- lift $ prompt "New weight (0 for bodyweight):"
      updateProficiency exer newreps newweight

addNewExercise :: (MonadException m) => FitStateT (InputT m) ()
addNewExercise = do
  name <- lift $ prompt "Exercise Name:"
  createExercise name

removeOldExercise :: (MonadException m) => FitStateT (InputT m) ()
removeOldExercise = do
  exercises <- exerciseList
  mexer <- liftIO $ inputMenu def "Known Exercises" exercises
  case mexer of
    MenuError -> liftIO $ printf "You don't have any exercises in this database yet to delete.\n"
    MenuInput exer -> do
      confirm <- lift $ prompt "Are you sure? (type yes):"
      if confirm /= ("yes" :: String)
        then return ()
        else deleteExercise exer
    MenuQuit -> return ()

addExerciseToWorkout :: (MonadIO m) => FitStateT m ()
addExerciseToWorkout = do
  exercises <- exerciseList
  workout <- currentWorkoutList
  let exercisesnotinworkout = exercises \\ workout
  mexer <- liftIO $ inputMenu def "Exercises Not In Workout" exercisesnotinworkout
  case mexer of
    MenuError -> liftIO $ printf "You already have every known exercise in your workout.\n"
    MenuInput exer -> do
      addToWorkout exer
    MenuQuit -> return ()
  
removeExerciseFromWorkout :: (MonadIO m) => FitStateT m ()
removeExerciseFromWorkout = do
  workout <- currentWorkoutList
  mexer <- liftIO $ inputMenu def "Exercises Not In Workout" workout
  case mexer of
    MenuError -> liftIO $ printf "You already have every known exercise in your workout.\n"
    MenuInput exer -> do
      remFromWorkout exer
      liftIO $ printf "%s has been removed from your workout.\n" exer
    MenuQuit -> return ()
 

  
  
{-
printWiki :: [(String, Int)] -> IO ()
printWiki = mapM_ printfunc
  where
    printfunc (exer, w) = do
      putChar '|'
      putStr exer
      putChar '|'
      putStr (intercalate "|" $ map show (progression w))
      putStrLn "|"



-}

{-
exerciseList :: Exercises
exerciseList = M.mapWithKey (\k l -> Exercise k l) $ M.fromList $ [
  ("squats"          ,"Squats"),
  ("deadlifts"       ,"Stiff Legged Dead"),
  ("inclinepress"    ,"Incline Press"),
  ("chinups"         ,"Chinups"),
  ("shrugs"          ,"Shrugs"),
  ("shoulderpress"   ,"Shoulder Press"),
  ("lateralraise"    ,"Lateral Raise"),
  ("reardeltraise"   ,"Rear Delt Raise"),
  ("curls"           ,"Curls"),
  ("tricepsextension","Triceps Extension"),
  ("calfraise"       ,"Calf Raise"),
  ("crunches"        ,"Crunches")]


-}
