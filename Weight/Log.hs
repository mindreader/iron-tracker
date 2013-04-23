{-# LANGUAGE OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Weight.Log(
  logLift, pastHistory, allHistory, dbFilterCurrentWorkout, dbAddExerciseToWorkout, dbRemExerciseFromWorkout
) where

import Data.Time

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


import Control.Applicative
import Control.Lens
import Control.Exception (bracket)

import Weight.Types
import Control.Monad.Trans (liftIO, MonadIO)

import Util

instance FromRow Proficiency where
  fromRow = Pro <$> field <*> (fmap toRational (field :: RowParser Double))

withDb :: MonadIO m => (Connection -> IO a) -> m a
withDb st = do
  statedir <- liftIO $ stateDir
  liftIO $ bracket (open (statedir++"/weight.db")) close st

logLift :: MonadIO m => Exercise -> (Int, Proficiency) -> m ()
logLift exer (attemptedreps, prof) = withDb $ \conn -> do
    execute conn
      "delete from weight_log where exercise_id = ? and whenit = date('now','localtime','-5 hour')"
      (Only $ exer ^. eExerciseId)
    execute conn
      "insert into weight_log (exercise_id, attempted_reps, reps, weight, whenit) values (?, ?,?,?,date('now','localtime','-5 hour'))"
      (exer ^. eExerciseId, attemptedreps, prof ^. pReps, fromRational (prof ^. pWeight) :: Double)

allHistory :: MonadIO m => Exercise -> Int -> m [(Day, (Reps, Proficiency))]
allHistory exer n = withDb $ \conn -> do
    logs <- query conn
      "select attempted_reps, reps, weight, whenit from weight_log where exercise_id = ? order by whenit desc limit ?"
      (exer ^. eExerciseId, n)
    return $ fmap (\(ar,r,w,d) -> (d,(ar, Pro r (toRational (w::Double))))) logs

pastHistory :: MonadIO m => Exercise -> Int -> m [(Day, (Reps, Proficiency))]
pastHistory exer n = withDb $ \conn -> do
    logs <- query conn
      "select attempted_reps, reps, weight, whenit from weight_log where exercise_id = ? and whenit <> date('now','localtime','-5 hour') order by whenit desc limit ?"
      (exer ^. eExerciseId, n)
    return $ fmap (\(ar,r,w,d) -> (d,(ar, Pro r (toRational (w::Double))))) logs


dbFilterCurrentWorkout :: MonadIO m => [Exercise] -> m [Exercise]
dbFilterCurrentWorkout exers = withDb $ \conn -> do
  ids <- fmap (fmap fromOnly) $ query_ conn "select exercise_id from current_workout"
  return $ filter (\x -> x ^. eExerciseId `elem` ids) exers

dbAddExerciseToWorkout :: MonadIO m => Exercise -> m ()
dbAddExerciseToWorkout exer = do
  dbRemExerciseFromWorkout exer  -- TODO opens and closes database unnecessarily.
  withDb $ \conn ->
    execute conn "insert into current_workout (exercise_id) values (?)" (Only $ exer ^. eExerciseId)

dbRemExerciseFromWorkout :: MonadIO m => Exercise -> m ()
dbRemExerciseFromWorkout exer = withDb $ \conn ->
  execute conn "delete from current_workout where exercise_id = ?" (Only $ exer ^. eExerciseId)
