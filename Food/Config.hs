{-# LANGUAGE OverloadedStrings #-}
module Food.Config(
  loadFoodConfig
) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Aeson (withObject)

import qualified Data.Text as T
import qualified Data.Map as M

import Control.Monad (mzero)
import Data.Yaml

import Data.Default

import Food.Types
import Food.Formulas

import Util

instance FromJSON FoodState where
  parseJSON (Object v) = do
    ingredients <- fmap (M.mapWithKey (\k v -> v { _iName = k })) $ v .: "ingredients" :: Parser (M.Map T.Text Ingredient)
    -- To facilitate the numerous amounts of foods that are just an ingredient, as well as piecemeal eating of things that
    -- are not really actual recipes, all ingredients will be listed with ingredient: prepended to the map of recipes.
    let ingredientsAsFood = M.mapKeys (\key -> "ingredient: " `T.append` key) . M.map (\ing -> [_iName ing]) $ ingredients

    foods <- v .: "foods" :: Parser (M.Map T.Text [T.Text])

    (cals, prot, fat, carbs) <- v .: "daily requirements" >>= withObject "daily requirements object" (\v ->
      (,,,) <$> v .:? "calories" <*> v .:? "protein" <*> v .:? "fat" <*> v .:? "carbs")

    return $ FS (M.mapWithKey (namesToIngredients ingredients) (foods `M.union` ingredientsAsFood) ) (Req cals prot fat carbs)
  parseJSON _ = mzero

namesToIngredients :: (M.Map T.Text Ingredient) -> T.Text -> [T.Text] -> Food
namesToIngredients ingredients foodName = Food foodName . loop
  where
    loop [] = []
    loop (ingName:is) = case M.lookup ingName ingredients of
      Just ing -> ing : loop is
      Nothing -> error $ "Ingredient " ++ T.unpack ingName ++ " does not exist in your food config file."

instance FromJSON Ingredient where
  parseJSON (Object v) = do
    ssize    <- v .:? "serving size" :: Parser (Maybe Int)
    snumber  <- v .:? "serving number" :: Parser (Maybe Int)
    protein  <- v .: "protein" :: Parser Int
    calories <- v .: "cals" :: Parser Int
    fat      <- v .: "fat" :: Parser Int
    return $ Ing undefined ssize snumber calories protein fat (calcCarbs calories protein fat)
  parseJSON _ = mzero
  
loadFoodConfig :: IO FoodState
loadFoodConfig = do
  statedir <- liftIO stateDir
  fmap (maybe def id) $ decodeFile (statedir ++ "/food.yaml")

