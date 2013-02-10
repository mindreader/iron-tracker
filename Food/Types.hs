{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module Food.Types where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as M

import Data.Monoid

import Data.Default

import Menu

type Calories = Int
type Fat = Int
type Protein = Int
type Carbs = Int

newtype Nutrition = Nut (Calories, Protein, Fat, Carbs) deriving Show

instance Monoid Nutrition where
  mempty = Nut (0,0,0,0)
  mappend (Nut (!c1, !p1, !f1, !ca1)) (Nut (c2, p2, f2, ca2)) = Nut (c1 + c2, p1 + p2, f1 +f2, ca1 + ca2)

data Ingredient = Ing {
  _iName :: T.Text,
  _iServingSize :: Maybe Int, -- serving size in grams
  _iServingNumber :: Maybe Int, -- typical number eaten
  _iCalories, _iProtein, _iFat, _iCarbs :: Int
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