{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Food where


import Prelude hiding (catch)

import Data.Acid
import Data.Acid.Local

import Data.SafeCopy

import Data.Data
import Data.Typeable



import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Reader

import System.Directory (getHomeDirectory, removeDirectoryRecursive)

import Control.Exception (catch)


type Protein = Int -- in grams
type Calories = Int -- in kcals

data FoodState = FoodState {
  foods :: [Food]
} deriving (Data, Typeable)

newtype FoodStateT m a = FoodStateT (StateT FoodState m a)
  deriving (Monad, MonadIO, MonadState FoodState, MonadTrans, Functor)

data Food = Food {
    name :: T.Text,
    protein :: Protein,
    calories :: Calories,
    typicalWeight :: Maybe Int, -- In grams
    typicalAmount :: Maybe Float -- ie. Number of oreos
  } | Recipe {
    ingredients :: [Food]
  } deriving (Data, Typeable)


myUpdate :: FoodState -> Update FoodState ()
myUpdate arg = put arg

myQuery :: Query FoodState FoodState
myQuery = ask

$(deriveSafeCopy 0 'base ''Food)
$(deriveSafeCopy 0 'base ''FoodState)
$(makeAcidic ''FoodState ['myUpdate, 'myQuery])

runFoodStateT :: MonadIO m => FoodStateT m a -> m a
runFoodStateT (FoodStateT f) = do
  st <- foodStateOpen
  (res, st') <- runStateT f st
  foodStateSave st'
  return res

-- TODO move into library
prepareStateDir = do
  statedir <- fmap (++ "/.fitstate") $ getHomeDirectory
  return $ statedir


foodStateOpen :: MonadIO m => m FoodState
foodStateOpen = liftIO $ do
  statedir <- prepareStateDir
  db <- openLocalStateFrom statedir (FoodState [])
  res <- query db MyQuery
  closeAcidState db
  return res


foodStateSave :: MonadIO m => FoodState -> m ()
foodStateSave st = liftIO $ do
  statedir <- prepareStateDir
  db <- openLocalStateFrom statedir (FoodState [])
  update db (MyUpdate st)
  createArchive db
  createCheckpointAndClose db
  (removeDirectoryRecursive $ statedir ++ "/Archive")  `catch` (\e -> do return (e :: IOError) ;  return ())



foodList :: Monad m => FoodStateT m [Food]
foodList = filterFood ""


-- TODO support more than equality.
filterFood :: Monad m => T.Text -> FoodStateT m [Food]
filterFood pat = do
  FoodState foods <- get
  return $ filter (\food -> (name food) == pat) foods

addFood :: Monad m => Food -> FoodStateT m ()
addFood food = do
  remFood (name food)
  modify (\(FoodState foods) -> FoodState $ food:foods)

remFood :: Monad m => T.Text -> FoodStateT m ()
remFood nametorem = modify (\(FoodState foods) -> FoodState $ filter (\food -> (name food) == nametorem) foods)
