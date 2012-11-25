{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad (when)
import Menu
import Data.Maybe (isJust)
import Data.List
import Data.Default

import IO
import FitState

--This one estimates suprisingly low on low rep ranges.
--oconnor :: Int -> Int -> Int -> Int
--oconnor r w r' = round $ (fromIntegral w) * (1+(0.025*(fromIntegral r))) / (1+(0.025 * (fromIntegral r')))

--This one is almost dead on for what I want.
--epley :: Int -> Int -> Int -> Int
--epley r w r' = round $ ((fromIntegral w * fromIntegral r / 30) + fromIntegral w) * (30 / (fromIntegral r'+30))


--progression :: Int -> [Int]
--progression = reverse . take 6 . iterate (\x -> round $ fromIntegral x * 0.95)

data MainMenuCommand = MMWorkoutStatus | MMUpdate | MMInclude | MMDisInclude | MMAdd | MMRemove deriving (Eq, Ord)


main :: IO ()
main = runFitStateT mainLoop


mainLoop :: MonadIO m => FitStateT m ()
mainLoop = do
  command <- liftIO $ inputMenu (def { quitOption = True }) "Main Menu" menuCrud

  case command of
    MenuError -> mainLoop
    MenuQuit -> return ()
    MenuInput command' -> do
      case command' of
        MMWorkoutStatus -> printWorkout
        MMInclude       -> addExerciseToWorkout
        MMDisInclude    -> removeExerciseFromWorkout
        MMUpdate        -> updateExercise
        MMAdd           -> addNewExercise
      continue

  where
    continue = pressAnyKey >> mainLoop
    menuCrud = [
      (MMWorkoutStatus, "Information on current workout "),
      (MMUpdate,        "Update an exercise in current workout"),
      (MMInclude,       "Add exercise to workout"),
      (MMDisInclude,    "Remove exercise from workout"),
      (MMAdd,           "Add New exercise"),
      (MMRemove,        "Remove exercise")]


fromList = undefined

printWorkout :: (Monad m, MonadIO m) => FitStateT m ()
printWorkout = do
  profs <- exercisesWithProfs currentWorkoutList
  when (null profs) $ io "You do not have any exercises set up in your workout.\n" ()
  mapM_ printExer profs
  where
    printExer (Exercise label, (Just (Proficiency weight reps))) = io "{}: {}@{}\n" ((left 20 ' ' label), reps,weight)
    printExer (Exercise label, Nothing) = io "{}: (none)\n" (Only (left 20 ' '  label))

exercisesWithProfs :: Monad m => FitStateT m [Exercise] -> FitStateT m [(Exercise, Maybe Proficiency)]
exercisesWithProfs f = f >>= mapM addProf
  where
    addProf exer@(Exercise label) = do
      prof <- getProficiency label
      return (exer, prof)


 
updateExercise :: (MonadIO m, Monad m) => FitStateT m ()
updateExercise = do
  workout <- currentWorkoutList

  mexer <- liftIO $ inputMenu def "Update Exercise" $ map (\(Exercise label) -> (label, label)) workout
  case mexer of
    MenuError -> io "You don't have any exercises set up for your workout.\n" ()
    MenuInput exer -> do
      mprof <- getProficiency exer
      case mprof of
        Nothing -> io "You have never done this exercise.\n" ()
        Just (Proficiency weight reps)-> io "You can currently do {} reps at {} pounds.\n" (reps, weight)
      newreps   <- prompt "New reps:" ()
      newweight <- prompt "New weight:" ()
      updateProficiency exer newreps newweight
    MenuQuit -> return ()

addNewExercise :: (MonadIO m, Monad m) => FitStateT m ()
addNewExercise = do
  name <- prompt "Exercise Name:" ()
  prof <- getProficiency name
  if isJust prof 
    then return ()
    else createExercise name 

removeExercise :: (MonadIO m, Monad m) => FitStateT m ()  
removeExercise = do
  exercises <- exerciseList
  mexer <- liftIO $ inputMenu def "Known Exercises" $ map (\(Exercise label) -> (label, label)) exercises
  case mexer of
    MenuError -> io "You don't have any exercises in this database yet to delete.\n" ()
    MenuInput exer -> do
      confirm <- prompt "Are you sure? (type yes):" ()
      if confirm /= ("yes" :: String)
        then return ()
        else deleteExercise exer
    MenuQuit -> return ()

addExerciseToWorkout :: (MonadIO m, Monad m) => FitStateT m ()  
addExerciseToWorkout = do
  exercises <- exerciseList
  workout <- currentWorkoutList
  let exercisesnotinworkout = exercises \\ workout
  mexer <- liftIO $ inputMenu def "Exercises Not In Workout" $ map (\(Exercise label) -> (label, label)) exercisesnotinworkout
  case mexer of
    MenuError -> io "You already have every known exercise in your workout.\n" ()
    MenuInput exer -> do
      addToWorkout exer
    MenuQuit -> return ()
  
removeExerciseFromWorkout :: (MonadIO m, Monad m) => FitStateT m ()  
removeExerciseFromWorkout = do
  workout <- currentWorkoutList
  mexer <- liftIO $ inputMenu def "Exercises Not In Workout" $ map (\(Exercise label) -> (label, label)) workout
  case mexer of
    MenuError -> io "You already have every known exercise in your workout.\n" ()
    MenuInput exer -> do
      remFromWorkout exer
      io "{} has been removed from your workout.\n" (Only exer)
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
