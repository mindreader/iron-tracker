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
import Safe (fromJustNote)

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
} deriving Show

type Exercises = M.Map T.Text Exercise

type Weight = Int
type Reps   = Int
data Proficiency = Proficiency Exercise (Weight, Reps) deriving Show

type Proficiencies = M.Map T.Text Proficiency


data FitState = FitState {
  proficiencies :: Proficiencies,
  exercises :: Exercises,
  workout :: Exercises
}

data MainMenuCommand = MMWorkoutStatus | MMUpdate | MMAdd | MMRemove | MMSave | MMQuit deriving (Eq, Ord)


main :: IO ()
main = do
  let fitstate = FitState profList exerciseList workoutList
  runStateT mainLoop fitstate >> return ()

  where
    workoutList = M.fromList . drop 2 . take 6 . M.toList $ exerciseList
    profList = M.fromList . map (\(key, exer) -> (key, Proficiency exer (5,6))) . drop 4 . take 9 . M.toList $ exerciseList


mainLoop :: StateT FitState IO ()
mainLoop = do
  command <- liftIO $ inputMenu "Main Menu" menuCrud

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
    menuCrud = [
      (MMWorkoutStatus, "Information on current workout "),
      (MMUpdate,        "Update an exercise in current workout"),
      (MMAdd,           "Add New exercise"),
      (MMRemove,        "Remove exercise"),
      (MMSave,          "Write to disk"),
      (MMQuit,          "Quit")]


fromList = undefined

printWorkout :: StateT FitState IO ()
printWorkout = fmap proficiencies get >>= DT.mapM printExer >> return ()
  where printExer (Proficiency exer (weight, reps)) = io "{}: {}@{}\n" ((left 20 ' ' $ exerLabel exer), reps,weight)
 
updateExercise :: StateT FitState IO () 
updateExercise = do
  state <- get
  let profs = proficiencies state                    :: Proficiencies
      exers = exercises state `M.intersection` profs :: Exercises
  
  mexer <- liftIO $ inputMenu "Current Workout" (M.toList $ M.map exerLabel (workout state)) :: StateT FitState IO (Maybe T.Text)
  case mexer of
    Nothing -> io "You don't have any exercises set up for your workout.\n" ()
    Just key -> do
      case M.lookup key profs of
        Nothing -> io "You have never done this exercise.\n" ()
        Just (Proficiency exer (weight, reps))-> io "For {}, you can do {} reps at {} pounds.\n" (exerLabel exer, reps, weight)
      newweight <- prompt "New weight:" () :: StateT FitState IO Int
      newreps   <- prompt "New reps:" () :: StateT FitState IO Int
      modify (\s -> s { proficiencies = M.insert key (Proficiency (fromJustNote "updateExercise" $ M.lookup key (exercises state)) (newweight, newreps)) profs })


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



