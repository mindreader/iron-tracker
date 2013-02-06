{-# LANGUAGE TemplateHaskell #-}

module Food.Types where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as M

import Data.Default

import Menu

type Calories = Int
type Fat = Int
type Protein = Int
type Carbs = Int

data Ingredient = Ing {
  _iName :: T.Text,
  _iServingSize :: Maybe Int, -- serving size in grams
  _iServingNumber :: Maybe Int, -- typical number eaten
  _iCalories, _iProtein, _iFat :: Int
} deriving Show

data Food = Food {
  _fName :: T.Text,
  _fIngredients :: [Ingredient]
} deriving Show

data FoodState = FS {
  _foods :: M.Map T.Text Food,
  _requirements :: Requirements
} deriving (Show)

data Requirements = Req {
  _rCalories :: Maybe Calories,
  _rProtein  :: Maybe Protein,
  _rFat      :: Maybe Fat,
  _rCarbs    :: Maybe Carbs
} deriving (Show)

instance Default FoodState where
  def = FS M.empty (Req Nothing Nothing Nothing Nothing)


makeLenses ''Ingredient
makeLenses ''Food
makeLenses ''FoodState
