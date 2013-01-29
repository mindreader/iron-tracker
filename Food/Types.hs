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
  _foods :: M.Map T.Text Food
} deriving (Show)

instance Default FoodState where
  def = FS M.empty


makeLenses ''Ingredient
makeLenses ''Food
makeLenses ''FoodState
