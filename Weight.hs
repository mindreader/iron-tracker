{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad (when)
import qualified Data.Text as T
import Menu
import Data.Maybe (isJust)
import Data.List
import Data.Default

import IO
import FitState

import System.IO (stdout, stdin, hSetBuffering, BufferMode(..))

import System.Console.Haskeline (MonadException, InputT, runInputT, defaultSettings)
import Data.Time (Day)

--This one estimates suprisingly low on low rep ranges.
--oconnor :: Int -> Int -> Int -> Int
--oconnor r w r' = round $ (fromIntegral w) * (1+(0.025*(fromIntegral r))) / (1+(0.025 * (fromIntegral r')))

--This one is almost dead on for what I want.
epley :: Int -> Int -> Int -> Int
epley r w r' = round $ ((fromIntegral w * fromIntegral r / 30) + fromIntegral w) * (30 / (fromIntegral r'+30))


data MainMenuCommand = MMWorkoutStatus | MMAdjustWorkoutReps | MMUpdate | MMInclude | MMDisInclude | MMAdd | MMRemove deriving (Eq, Ord)


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
        MMAdjustWorkoutReps -> adjustWorkoutByReps
        MMWorkoutStatus     -> printWorkout id
        MMInclude           -> addExerciseToWorkout
        MMDisInclude        -> removeExerciseFromWorkout
        MMUpdate            -> updateExercise
        MMAdd               -> addNewExercise
        MMRemove            -> removeOldExercise
      continue

  where
    continue = pressAnyKey >> mainLoop
    menuCrud = [
      (MMAdjustWorkoutReps, "Workout with new rep count"),
      (MMWorkoutStatus,     "Information on current workout "::T.Text),
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
  where

    printExer (Exercise label, (date,prof)) =
      printf "%-26s %-8s %s\n" (labelJustify label) (weightRepJustify prof) (dateJustify date)
      where
        labelJustify :: PrintfArg a => a-> String
        labelJustify label = printf "%s:" label

        weightRepJustify :: Maybe Proficiency -> String
        weightRepJustify  Nothing                         = "(none)"
        weightRepJustify (Just (Proficiency 0 reps))      = printf "%d" reps
        weightRepJustify (Just (Proficiency weight reps)) = printf "%d@%d" reps weight

        dateJustify Nothing     = ""
        dateJustify (Just date) = show date

    exercisesWithInfo f = f >>= mapM addInfo

    addInfo exer@(Exercise label) = do
      prof <- getProficiency label
      lastworkout <- getLastWorkout label
      return (exer, (lastworkout, prof))

adjustWorkoutByReps :: (MonadException m) => FitStateT (InputT m) ()
adjustWorkoutByReps = do
  newreps <- lift $ prompt "Reps you want to do:"
  printWorkout $ \(Proficiency weight reps) -> Proficiency (epley reps weight newreps) newreps



 
updateExercise :: (MonadException m) => FitStateT (InputT m) ()
updateExercise = do
  workout <- currentWorkoutList

  mexer <- liftIO $ inputMenu def "Update Exercise" workout
  case mexer of
    MenuError -> liftIO $ printf "You don't have any exercises set up for your workout.\n"
    MenuInput exer -> do
      mprof <- getProficiency exer
      case mprof of
        Nothing -> liftIO $ printf "You have never done this exercise.\n"
        Just (Proficiency weight reps)-> liftIO $ printf "You can currently do %d reps at %d pounds.\n" reps weight
      newreps   <- lift $ prompt "New reps:"
      newweight <- lift $ prompt "New weight (0 for bodyweight):"
      updateProficiency exer newreps newweight
    MenuQuit -> return ()

addNewExercise :: (MonadException m) => FitStateT (InputT m) ()
addNewExercise = do
  name <- lift $ prompt "Exercise Name:"
  prof <- getProficiency name
  if isJust prof 
    then return ()
    else createExercise name 

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
