{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Data.Traversable as DT
import IO
import Data.Monoid
import qualified Data.Map as M
import System.Exit (exitSuccess)
import qualified Data.Text as T
import Menu

--main = (fmap (zip exerList) . mapM query) exerList >>= printWiki



--This one estimates suprisingly low on low rep ranges.
--oconnor :: Int -> Int -> Int -> Int
--oconnor r w r' = round $ (fromIntegral w) * (1+(0.025*(fromIntegral r))) / (1+(0.025 * (fromIntegral r')))

--This one is almost dead on for what I want.
--epley :: Int -> Int -> Int -> Int
--epley r w r' = round $ ((fromIntegral w * fromIntegral r / 30) + fromIntegral w) * (30 / (fromIntegral r'+30))


--progression :: Int -> [Int]
--progression = reverse . take 6 . iterate (\x -> round $ fromIntegral x * 0.95)

data Exercise = Exercise {
  exerKey   :: T.Text,
  exerLabel :: T.Text
}

type Exercises = M.Map T.Text Exercise

type Weight = Int
type Reps   = Int
data Proficiency = Proficiency Exercise (Weight, Reps)

type Proficiencies = M.Map T.Text Proficiency


data FitState = FitState {
  proficiencies :: Proficiencies,
  exercises :: Exercises,
  workout :: Exercises
}

data MainMenuCommand = MMWorkoutStatus | MMUpdate | MMAdd | MMRemove | MMSave | MMQuit


main :: IO ()
main = do
  let fitstate = FitState M.empty exerciseList M.empty
  runStateT mainLoop fitstate >> return ()


mainLoop :: StateT FitState IO ()
mainLoop = do
  command <- liftIO $ inputMenu menuCrud

  case command of
    Nothing -> mainLoop
    Just command' -> case command' of
      MMWorkoutStatus -> printWorkout
      MMUpdate        -> updateExercise
      MMAdd           -> addExercise
      MMRemove        -> removeExercise
      MMSave          -> saveExercises
      MMQuit          -> liftIO $ exitSuccess

  io "Press any key to continue" () >> liftIO getLine
  mainLoop
  where
    menuCrud :: Menu MainMenuCommand
    menuCrud = fromList "Exercise Tracking Program" $ [
      (MMWorkoutStatus, "Information on current workout "),
      (MMUpdate,        "Update an exercise in current workout"),
      (MMAdd,           "Add New exercise"),
      (MMRemove,        "Remove exercise"),
      (MMSave,          "Write to disk"),
      (MMQuit,          "Quit")]



printWorkout :: StateT FitState IO ()
printWorkout = fmap proficiencies get >>= DT.mapM printExer >> return ()
  where printExer (Proficiency exer _) = io "{}\n" (Only $ exerLabel exer)
 
updateExercise :: StateT FitState IO () 
updateExercise = do
  state <- get
  let profs = proficiencies state :: Proficiencies
      exers = exercises state `M.intersection` profs
  
  exer <- liftIO . inputMenu $ fromList "Current Workout" (M.toList exers)
  undefined 
{-  case lookup exer profs of
        Nothing -> error "This shouldn't happen"
        Just (label, repinfo) -> do
          io "Current Status of {}\n" (Only label)
          case repinfo of
            Nothing ->            io "You have never done this exercise.\n" ()
            Just (weight,reps) -> io "You can do {} at {} pounds.\n" (reps,weight)
  where
    noExercises = io "Your current workout has no exercises in it.\n" ()
-}

  
    
addExercise = error "addExercise"
removeExercise = error "removeExercise"
saveExercises = error "saveExercises"

--menuExercises :: Menu T.Text
--menuExercises = fromList "Exercise List" $ map (\(Exercise key label) -> (key,label)) exerciseList

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



