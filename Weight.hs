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
import Control.Applicative

import Weight.Formulas
import Weight.Config
import Weight.Types
import Weight.Log
import qualified Weight.PlateCalc as PC
import Weight.PlateOrder as PO

import Menu
import IO

data WeightMenuCommand = MWWorkoutMode | MWWorkoutStatus | MWAdjustWorkoutReps | MWUpdate | MWInclude | MWDisInclude deriving (Eq, Ord)

newtype App a = App (StateT AppState IO a)
  deriving (Monad, MonadState AppState, MonadIO, Functor, Applicative)

data AppState = AS {
  _weightState :: WeightState,
  _wCycle :: Integer,
  _today :: Day
}

makeLenses ''AppState

runWeightRoutine :: IO ()
runWeightRoutine = runApp mainLoop

runApp :: App a -> IO a
runApp (App s) = do
  today <- fmap (localDay . zonedTimeToLocalTime) getZonedTime
  ws <- loadWeightConfig
  evalStateT s (AS ws 12 today)


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


-- On `Day` you were told to do `Reps` but you were able to do (Proficiency Reps Weight).
type History = [(Day, (Reps, Proficiency))]


workoutMode :: App ()
workoutMode = do
  workout <- currentWorkout
  wCycle <- use wCycle
  today <- use today
  (exers, histories, plates, tryThis) <- fmap L.unzip4 $ mapM (fetchPlateConfig wCycle today) workout :: App ([Exercise], [History], [[PO.Plate]], [TryThis])
  workoutMode' [] (L.zip3 exers histories tryThis) plates
  liftIO $ printf "Workout complete.\n"
  where
      
    fetchPlateConfig :: Integer -> Day -> Exercise -> App (Exercise, History, [PO.Plate], TryThis)
    fetchPlateConfig wCycle today exer = do
      history <- pastHistory exer 5
      let tryThis@(TryThis _ tw) = suggestNewRepWeight wCycle today $ map (\(date,(attemptedReps,Pro r w)) -> DidThis attemptedReps r w date) history
      return $ (exer, history, case PC.plateCalc PC.Barbell tw of PC.Plates ps -> plateOrder2Calc ps, tryThis)

    -- TODO move to general purpose plate library for cleaner interface.
    plateOrder2Calc :: [PC.Plate] -> [PO.Plate]
    plateOrder2Calc (PC.P45  n :ps) = replicate (n `div` 2) PO.P45  ++ plateOrder2Calc ps
    plateOrder2Calc (PC.P25  n :ps) = replicate (n `div` 2) PO.P25  ++ plateOrder2Calc ps
    plateOrder2Calc (PC.P10  n :ps) = replicate (n `div` 2) PO.P10  ++ plateOrder2Calc ps
    plateOrder2Calc (PC.P5   n :ps) = replicate (n `div` 2) PO.P5   ++ plateOrder2Calc ps
    plateOrder2Calc (PC.P2p5 n :ps) = replicate (n `div` 2) PO.P2p5 ++ plateOrder2Calc ps
    plateOrder2Calc _ = []

    workoutMode' :: [PO.Plate] -> [(Exercise, History, TryThis)] -> [[PO.Plate]] -> App ()
    workoutMode' _ [] _ = return ()
    workoutMode' lastPlateOrder ((exer,history,(TryThis tr tw)):xs) plates@(_:ps) = do
      liftIO $ printf "\n%s\n" (exer ^. eName)
      printHistory history
      let thisPlateOrder = optimalPlateOrder lastPlateOrder $ plates
      liftIO $ case exer ^. eType of
        Barbell -> printf "You must do %s.\n" (formatRepsWeight tr tw (Just $ PO.displayPlates thisPlateOrder))
        Bodyweight -> printf "You must do whatever.\n"
        Dumbbell -> printf "You must do however many at %d" (999::Int)
      inputReps >>= (\reps -> logLift exer (tr, Pro reps tw))
      workoutMode' thisPlateOrder xs ps

    printHistory :: History -> App ()
    printHistory history = do
      mapM_ printHistory' (L.reverse history)
      where
        printHistory' (day, (attempted,pro)) = liftIO $ printf " %s : tried %d did %s\n" (show day) attempted (formatRepsWeight (pro ^. pReps) (pro ^. pWeight) Nothing)

    formatRepsWeight :: Reps -> Weight -> (Maybe String) -> String
    -- TODO this really should suggest the last weight we did.  If we did it over two weeks ago, it doesn't have the info at this point in code.
    formatRepsWeight reps 0.0 _ = printf "%d" reps
    formatRepsWeight reps weight (Just plates) = printf "%d@%s (%s)" reps (rTrimZeros $ show $ (fromRational weight :: Double)) plates
    formatRepsWeight reps weight Nothing = printf "%d@%s (%s)" reps (rTrimZeros $ show $ (fromRational weight :: Double)) (PC.displayPlateCalc PC.Barbell weight)

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

