{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module Food.Types(
  Calories, Fat, Protein, Carbs, Nutrition(..),
  NutritionInfo(..),nServingSize,nServingNumber,nCalories,nProtein,nCarbs,nFat,
  Food(Ingredient,Recipe),_fName, _fTypical,fName,fTypical,fNutritionInfo,fIngredients,
  FoodState(..),foods,requirements,
  Requirements(..),rCalories,rFat,rProtein,rCarbs,
) where

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
  mappend (Nut !c1 !p1 !f1 !ca1) (Nut !c2 !p2 !f2 !ca2) = Nut (c1 + c2) (p1 + p2) (f1 + f2) (ca1 + ca2)

data NutritionInfo = NI {
  _nServingSize   :: Maybe Int, -- ^ serving size in grams (defaults to whocares)
  _nServingNumber :: Int,       -- ^ number required to get this info (defaults to one)
  _nCalories :: Calories,
  _nProtein  :: Protein,
  _nCarbs    :: Carbs,
  _nFat      :: Fat
} deriving Show

data Food =
  Ingredient {
    _fName :: Text,   -- ^ name
    _fTypical :: Int, -- ^ number of this item typically eaten in a meal
    _fNutritionInfo :: NutritionInfo -- ^ Some are just recipe names, others are concrete ingredients with health info
  } |
  Recipe {
    _fName :: Text,      -- ^ name
    _fTypical :: Int,    -- ^ number of this item typically eaten in a meal
    _fIngredients :: [Food] -- ^ A list of ingredients, or recipes that are part of this recipe.
  }
  deriving Show


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


makeLenses ''FoodState
makeLenses ''Food
makeLenses ''NutritionInfo
makeLenses ''Requirements

{-
food1 = Ingredient "cheese" 1 (NI Nothing 1 50 1 0 5)
food2 = Ingredient "bread" 1 (NI (Just 10) 1 50 0 12 0)
food3 = Recipe "grilledcheese" 1 [food1,food2]
-}
