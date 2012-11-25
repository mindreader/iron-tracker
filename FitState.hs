{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}

module Main where

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


fitStateOpen :: MonadIO m => FitStateT m ()
fitStateOpen = do
  db <- liftIO $ openLocalState (FitState M.empty)
  (liftIO $ query db MyQuery) >>= put


runFitStateT :: MonadIO m => FitStateT m a -> m ()
runFitStateT f = fitStateOpen >> f >> fitStateSave >> return ()



fitStateSave :: MonadIO m => FitStateT m ()
fitStateSave = do
  db <- liftIO $ openLocalState (FitState M.empty)
  st <- get
  liftIO $ (update db (MyUpdate st))

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
