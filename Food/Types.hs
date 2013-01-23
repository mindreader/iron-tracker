{-# LANGUAGE TemplateHaskell #-}

module Food.Types where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as M

import Menu

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

data FoodState = WS {
  _foods :: M.Map T.Text Food
} deriving (Show)


makeLenses ''Ingredient
makeLenses ''Food
makeLenses ''FoodState
