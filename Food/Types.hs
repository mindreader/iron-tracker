{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module Food.Types where

import BasicPrelude

import Control.Lens

import Data.Default
import Data.Tree

type Calories = Int
type Fat = Int
type Protein = Int
type Carbs = Int

data Nutrition = Nut Calories Protein Fat Carbs deriving Show

instance Monoid Nutrition where
  mempty = Nut 0 0 0 0
  mappend (Nut !c1 !p1 !f1 !ca1) (Nut c2 p2 f2 ca2) = Nut (c1 + c2) (p1 + p2) (f1 + f2) (ca1 + ca2)

data NutritionInfo = NI {
  _servingSize   :: Maybe Int, -- ^ serving size in grams (defaults to unknown)
  _servingNumber :: Int,       -- ^ number required to get this info (defaults to one)
  _calories :: Calories,
  _protein  :: Protein,
  _carbs    :: Carbs,
  _fat      :: Fat
} deriving Show

data Food = Food {
    _fName :: Text,           -- ^ name
    _fTypicalPerMeal :: Int,  -- ^ number of this item typically eaten in a meal
    _fNutritionInfo :: Maybe NutritionInfo -- ^ Some are just recipe names, others are concrete ingredients with health info
  } deriving Show


-- | Foods are basically a tree of food items.
-- All the leaves should be ingredients, and have nutrition info
-- everything else should be a recipe which is composed of all the things beneath it in the tree
-- The root node could just be an ingredient.  They are indexed by their toplevel name.
data FoodState = FS {
  _foods :: Map Text Food,
  _requirements :: Requirements
} deriving (Show)

data Requirements = Req {
  _rCalories :: Maybe Calories,
  _rProtein  :: Maybe Protein,
  _rFat      :: Maybe Fat,
  _rCarbs    :: Maybe Carbs
} deriving (Show)

instance Default FoodState where
  def = FS empty (Req Nothing Nothing Nothing Nothing)


makeLenses ''Food
makeLenses ''FoodState
