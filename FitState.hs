{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}

module FitState where

import Data.Acid
import Data.Acid.Advanced

import Data.SafeCopy

import Data.Typeable
import Data.Data

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Text as T

newtype FitStateT m a = FitStateT (StateT FitState m a)
  deriving (Monad, MonadIO, MonadState FitState)

 
newtype FitState = FitState (M.Map T.Text FitInfo) deriving (Data, Typeable)

data FitInfo = FitInfo {
  exercise :: Exercise,
  proficiency :: Maybe Proficiency,
  inWorkout :: Bool
} deriving (Data, Typeable)

data Exercise = Exercise {
  exerKey   :: T.Text,
  exerLabel :: T.Text
} deriving (Show, Data, Typeable)

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
 

fitStateOpen :: MonadIO m => m FitState
fitStateOpen = liftIO $ do
  db <- openLocalState (FitState M.empty)
  res <- query db MyQuery
  closeAcidState db
  return res


fitStateSave :: MonadIO m => FitState -> m ()
fitStateSave st = liftIO $ do
  db <- openLocalState (FitState M.empty)
  update db (MyUpdate st)
  closeAcidState db



exerciseList :: Monad m => FitStateT m [Exercise]
exerciseList = do
  FitState st <- get
  return $ map exercise $ M.elems st

currentWorkoutList :: Monad m => FitStateT m [Exercise]
currentWorkoutList = do
  FitState st <- get
  return $ map exercise . filter inWorkout . M.elems $ st
  
addExercise :: Monad m => T.Text -> T.Text -> FitStateT m ()
addExercise key label = modify (\(FitState st) ->  FitState $ M.insert key (FitInfo (Exercise key label) Nothing False) st)

remExercise :: Monad m => T.Text -> FitStateT m ()
remExercise key = modify (\(FitState st) -> FitState $ M.delete key st)

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
