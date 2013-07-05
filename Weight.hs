{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Weight(
  runWeightRoutine
) where

import BasicPrelude

import Control.Lens
import Data.Time (Day)

import qualified Data.Text as T (length, pack, unpack)
import qualified Data.Map as M (elems)

import qualified Data.Text as T (unpack)

import Data.Default
import Safe (headMay)

import Control.Monad.State

import Weight.Formulas
import Weight.Config
import Weight.Types
import Weight.Log
import qualified Weight.PlateCalc as PC
import Weight.PlateOrder as PO
import WorkoutPlan

import Menu
import IO

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
      (MWWorkoutStatus,     "Current workout proficiencies"::Text),
      (MWUpdate,            "Update an exercise in current workout"),
      (MWInclude,           "Add exercise to workout"),
      (MWDisInclude,        "Remove exercise from workout")]


-- !On `Day` you were told to do `Reps` but you were able to do (Proficiency Reps Weight).
type History = [(Day, (Reps, Proficiency))]

workoutMode :: App ()
workoutMode = do
  ws <- use weightState
  cycle <- use wCycle
  (Plan steps) <- liftIO $ workoutPlan ws cycle
  forM_ steps $ \step -> do

    let (exer, reps, weight) = case step of
          BodyWeightExercise e r  -> (e,r,0)
          DumbbellExercise e r w  -> (e,r,w)
          BarbellExercise e r w _ -> (e,r,w)

    beforeStep exer
    workoutStep step >> return ()
    afterStep exer reps weight
  liftIO $ printf "Workout complete.\n"
  where
    workoutStep (BarbellExercise exer reps weight plates) = do
      liftIO $ printf "You must do %s.\n" (formatRepsWeight reps weight (Just $ PO.displayPlates plates))
    workoutStep (DumbbellExercise exer reps weight) = do
      liftIO $ printf "You must do %s.\n" (formatRepsWeight reps weight Nothing)
    workoutStep (BodyWeightExercise _ reps) = do
      liftIO $ printf "You must do %d reps.\n" reps

    beforeStep exer = liftIO (printf "%s" (exer ^. eName)) >> liftIO (pastHistory exer 5) >>= printHistory
    afterStep exer toldReps toldWeight = do
      complete <- liftIO $ yesnoPrompt "Completed Exercise?" DefYes
      if complete
        then do
          newtoldReps <- inputRepAttempt toldReps
          newReps <- inputReps toldReps
          newWeight <- inputWeight toldWeight
          logLift exer (
            if newtoldReps == 0 then toldReps else newtoldReps,
            Pro
              (if newReps == 0 then toldReps else newReps)
              (if newWeight == 0 then toldWeight else newWeight))
        else return ()


    formatRepsWeight :: Reps -> Weight -> (Maybe Text) -> Text
    -- TODO this really should suggest the last weight we did.  If we did it over two weeks ago, it doesn't have the info at this point in code.
    formatRepsWeight reps 0.0 _ = show $ (printf "%d" reps :: String)
    formatRepsWeight reps weight (Just plates) = show $ (printf "%d@%s (%s)" reps (rTrimZeros $ show $ (fromRational weight :: Double)) plates :: String)
    formatRepsWeight reps weight Nothing = show $ (printf "%d@%s (%s)" reps (rTrimZeros $ show $ (fromRational weight :: Double)) (PC.displayPlateCalc PC.Barbell weight) :: String)

    printHistory :: History -> App ()
    printHistory history = do
      mapM_ printHistory' (reverse history)
      where
        printHistory' (day, (attempted,pro)) = liftIO $ printf " %s : tried %d did %s\n" (show day) attempted (formatRepsWeight (pro ^. pReps) (pro ^. pWeight) Nothing)


inputRepAttempt :: Int-> App Reps
inputRepAttempt def = liftIO . prompt $ "Attempted Reps (0 for " <> show def <> "):"
inputReps :: Int-> App Reps
inputReps 0 = liftIO . prompt $ "New reps:"
inputReps def = liftIO . prompt $ "New reps (0 for " <> show def <> "):"

-- FIXME can't enter weights that are not whole numbers.
inputWeight :: Rational -> App Weight
inputWeight 0 = fmap fromIntegral $ liftIO $ (prompt $ "New weight:" :: IO Int)
inputWeight def = fmap fromIntegral $ liftIO $ (prompt $ "New weight (0 for " <> show (round def) <> "):" :: IO Int)


inputProficiency :: App Proficiency
inputProficiency = Pro <$> inputReps 0 <*> inputWeight 0


exerciseList = use (weightState . exercises . to M.elems)
currentWorkout = fmap sort $ exerciseList >>= dbFilterCurrentWorkout

printWorkout :: (Proficiency -> Proficiency) -> App ()
printWorkout adjuster = do
  exers <- currentWorkout
  let maxExerNameLen = T.length $ maximumBy (\x y -> compare (T.length (x ^. eName)) (T.length (y ^. eName))) exers ^. eName
  when (null exers) $ liftIO $ printf "You do not have any exercises set up in your workout.\n"
  mapM exerWithProf exers >>= mapM_ (liftIO . putStrLn . formatLine maxExerNameLen)
  where
    exerWithProf :: Exercise -> App (Either Exercise (Day, Proficiency, Exercise))
    exerWithProf exer = do
      last <- allHistory exer 1
      case headMay last of
        Nothing          -> return . Left $ exer
        Just (day, (_, prof)) -> return . Right $ (day, adjuster prof, exer)


    formatLine :: Int -> Either Exercise (Day, Proficiency, Exercise) -> Text
    formatLine namelen (Left exercise) = T.pack $ printf
      ("%" <> (textToString . show $ namelen) <> "s : (never done)") (exercise ^. eName)
    formatLine namelen (Right (_, Pro reps 0.0, exercise)) = T.pack $ printf
      ("%" <> (textToString . show $ namelen) <> "s : %d") (exercise ^. eName) reps
    formatLine namelen (Right (_, Pro reps weight, exercise)) = T.pack $ printf
      ("%" <> (textToString . show $ namelen) <> "s : %d@%.3s (%s)") (exercise ^. eName) reps (rTrimZeros $ show $ fromRational weight) (PC.displayPlateCalc PC.Barbell weight)

rTrimZeros :: Text -> String
rTrimZeros = reverse . dropWhile (=='.') . dropWhile (== '0') . reverse . textToString

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
  let exercisesnotinworkout = exercises \\ workout
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

