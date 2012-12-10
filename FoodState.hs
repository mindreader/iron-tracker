{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module FoodState where


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
import Text.Regex.Posix ((=~))


type Protein  = Int -- in grams
type Fat      = Int -- in grams
type Carbs    = Int -- in grams
type Calories = Int -- in kcals
type Weight   = Int -- in grams

data FoodState = FoodState {
  foods :: [Food]
} deriving (Data, Typeable)

newtype FoodStateT m a = FoodStateT (StateT FoodState m a)
  deriving (Monad, MonadIO, MonadState FoodState, MonadTrans, Functor)

data Food = Food {
    name :: T.Text,
    protein      :: Protein,
    fat          :: Fat,
    carbs        :: Carbs,
    weightBased  :: Maybe Weight,
    amountBased   :: Bool
  } | Recipe {
    ingredients :: [Food]
  } deriving (Data, Typeable, Show, Eq)

calories :: Food -> Calories
calories (Food _ p f c _ _) = 4*p + 4*c + 9*f

instance Ord Food where
  compare food1 food2 = compare (name food1) (name food2)


myUpdate :: FoodState -> Update FoodState ()
myUpdate arg = put arg

myQuery :: Query FoodState FoodState
myQuery = ask

$(deriveSafeCopy 1 'base ''Food)
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
foodList = do
  FoodState foods <- get
  return foods

addFood :: Monad m => Food -> FoodStateT m ()
addFood food = do
  remFood (name food)
  modify (\(FoodState foods) -> FoodState $ food:foods)

remFood :: Monad m => T.Text -> FoodStateT m ()
remFood nametorem = modify (\(FoodState foods) -> FoodState $ filter (\food -> (name food) /= nametorem) foods)
