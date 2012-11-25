{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}

module FitState where

import Prelude hiding (catch)
import Data.Acid
import Data.Acid.Local

import Data.SafeCopy

import Data.Typeable
import Data.Data

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Text as T

import System.Directory (getHomeDirectory, removeDirectoryRecursive)

import Control.Exception (catch)

newtype FitStateT m a = FitStateT (StateT FitState m a)
  deriving (Monad, MonadIO, MonadState FitState)

 
newtype FitState = FitState (M.Map T.Text FitInfo) deriving (Data, Typeable)

data FitInfo = FitInfo {
  exercise :: Exercise,
  proficiency :: Maybe Proficiency,
  inWorkout :: Bool
} deriving (Data, Typeable)

newtype Exercise = Exercise T.Text deriving (Eq, Show, Data, Typeable)

type Weight = Int
type Reps   = Int
data Proficiency = Proficiency Weight Reps deriving (Show, Data, Typeable)

myUpdate :: FitState  -> Update FitState ()
myUpdate arg = put arg

myQuery :: Query FitState FitState
myQuery = ask

$(deriveSafeCopy 0 'base ''FitState)
$(deriveSafeCopy 0 'base ''FitInfo)
$(deriveSafeCopy 0 'base ''Exercise)
$(deriveSafeCopy 0 'base ''Proficiency)
$(makeAcidic ''FitState ['myUpdate, 'myQuery])


runFitStateT :: MonadIO m => FitStateT m a -> m a
runFitStateT (FitStateT f) = do
  st <- fitStateOpen
  (res, st') <- runStateT f st
  fitStateSave st'
  return res


prepareStateDir = do
  statedir <- fmap (++ "/.fitstate") $ getHomeDirectory
  return $ statedir

 

fitStateOpen :: MonadIO m => m FitState
fitStateOpen = liftIO $ do
  statedir <- prepareStateDir
  db <- openLocalStateFrom statedir (FitState M.empty)
  res <- query db MyQuery
  closeAcidState db
  return res


fitStateSave :: MonadIO m => FitState -> m ()
fitStateSave st = liftIO $ do
  statedir <- prepareStateDir
  db <- openLocalStateFrom statedir (FitState M.empty)
  update db (MyUpdate st)
  createArchive db
  createCheckpointAndClose db
  (removeDirectoryRecursive $ statedir ++ "/Archive")  `catch` (\e -> do return (e :: IOError) ;  return ())



exerciseList :: Monad m => FitStateT m [Exercise]
exerciseList = do
  FitState st <- get
  return $ map exercise $ M.elems st

currentWorkoutList :: Monad m => FitStateT m [Exercise]
currentWorkoutList = do
  FitState st <- get
  return $ map exercise . filter inWorkout . M.elems $ st
  
createExercise :: Monad m => T.Text -> FitStateT m ()
createExercise label = modify (\(FitState st) ->  FitState $ M.insert label (FitInfo (Exercise label) Nothing False) st)

deleteExercise :: Monad m => T.Text -> FitStateT m ()
deleteExercise label = modify (\(FitState st) -> FitState $ M.delete label st)

addToWorkout :: Monad m => T.Text -> FitStateT m ()
addToWorkout key = modify (\(FitState st) -> FitState $ M.adjust (\ fi -> fi { inWorkout = True }) key st)

remFromWorkout :: Monad m => T.Text -> FitStateT m ()
remFromWorkout key = modify (\(FitState st) -> FitState $ M.adjust (\ fi -> fi { inWorkout = False }) key st)

updateProficiency :: Monad m => T.Text -> Int -> Int -> FitStateT m ()
updateProficiency key reps weight = modify (\(FitState st) -> FitState $ M.adjust (\fi -> fi { proficiency = Just (Proficiency reps weight) }) key st)


getProficiency :: Monad m => T.Text -> FitStateT m (Maybe Proficiency)
getProficiency key = do
  FitState st <- get
  return . join . fmap proficiency . M.lookup key $ st
