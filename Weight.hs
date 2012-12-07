{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad (when)
import qualified Data.Text as T
import Menu
import Data.List ((\\))
import Data.Default (def)

import PlateCalc

import IO
import FitState

import System.Console.Haskeline (MonadException, InputT, runInputT, defaultSettings)
import Data.Time (Day)

import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))


data MainMenuCommand = MMWorkoutMode | MMWorkoutStatus | MMAdjustWorkoutReps | MMUpdate | MMInclude | MMDisInclude | MMAdd | MMRemove deriving (Eq, Ord)


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout LineBuffering
  runInputT defaultSettings (runFitStateT mainLoop)


mainLoop :: (MonadException m) => FitStateT (InputT m) ()
mainLoop = do
  command <- liftIO $ inputMenu (def { quitOption = True }) "Main Menu" menuCrud

  case command of
    MenuError -> mainLoop
    MenuQuit -> return ()
    MenuInput command' -> do
      case command' of
        MMWorkoutMode       -> workoutMode
        MMWorkoutStatus     -> printWorkout id
        MMAdjustWorkoutReps -> adjustWorkoutByReps
        MMInclude           -> addExerciseToWorkout
        MMDisInclude        -> removeExerciseFromWorkout
        MMUpdate            -> updateExercise
        MMAdd               -> addNewExercise
        MMRemove            -> removeOldExercise
      continue

  where
    continue = pressAnyKey >> mainLoop
    menuCrud = [
      (MMWorkoutMode,       "Perform a Workout"),
      (MMAdjustWorkoutReps, "Print workout with new rep count"),
      (MMWorkoutStatus,     "Current workout proficiencies"::T.Text),
      (MMUpdate,            "Update an exercise in current workout"),
      (MMInclude,           "Add exercise to workout"),
      (MMDisInclude,        "Remove exercise from workout"),
      (MMAdd,               "Add New exercise"),
      (MMRemove,            "Remove exercise")]


printWorkout :: (MonadIO m) => (Proficiency -> Proficiency) -> FitStateT (InputT m) ()
printWorkout f = do
  exers <- exercisesWithInfo currentWorkoutList
  when (null exers) $ liftIO $ printf "You do not have any exercises set up in your workout.\n"
  mapM_ (liftIO . printExer) $ fmap (fmap (fmap (fmap f))) exers


exercisesWithInfo :: Monad m => FitStateT m [Exercise] -> FitStateT m [(Exercise, (Maybe Day, Maybe Proficiency))]
exercisesWithInfo f = f >>= mapM addInfo
  where

    addInfo exer@(Exercise label) = do
      prof <- getProficiency label
      lastworkout <- getLastWorkout label
      return (exer, (lastworkout, prof))

printExer (Exercise label, (date,prof)) =
  printf "%-26s %-8s %s\n" (exerLabel label) (printMaybeProficiency prof) (printMaybeDate date)
  where
    exerLabel :: PrintfArg a => a-> String
    exerLabel label = printf "%s:" label


printMaybeProficiency :: Maybe Proficiency -> String
printMaybeProficiency  Nothing                         = "(none)"
printMaybeProficiency (Just (Proficiency 0 reps))      = printf "%d" reps
printMaybeProficiency (Just (Proficiency weight reps)) = printf "%d@%d (%s)" reps weight (displayPlateCalc weight)

printMaybeDate Nothing     = ""
printMaybeDate (Just date) = show date




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
          liftIO $ printf "For %s you were last able to do %s on %s.\n" exer (printMaybeProficiency mprof) (printMaybeDate mdate)
          liftIO $ printf "You must do %s.\n" (printMaybeProficiency newmprof)
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

addExerciseToWorkout :: (MonadIO m) => FitStateT (InputT m) ()
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
  
removeExerciseFromWorkout :: (MonadIO m) => FitStateT (InputT m) ()
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
