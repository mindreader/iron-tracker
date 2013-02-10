{-# LANGUAGE OverloadedStrings #-}
module Food.Config(
  loadFoodConfig
) where

import Control.Applicative
import Control.Monad.Trans.Maybe

import Data.Aeson (withObject)

import qualified Data.Text as T
import qualified Data.Map as M

import Control.Monad (mzero)
import Data.Yaml

import Data.Default

import Food.Types
import Food.Formulas

instance FromJSON FoodState where
  parseJSON (Object v) = do
    ingredients <- fmap (M.mapWithKey (\k v -> v { _iName = k })) $ v .: "ingredients" :: Parser (M.Map T.Text Ingredient)
    foods <- v .: "foods" :: Parser (M.Map T.Text [T.Text])
    (cals, prot, fat, carbs) <- v .: "daily requirements" >>= withObject "daily requirements object" (\v ->
      (,,,) <$> v .:? "calories" <*> v .:? "protein" <*> v .:? "fat" <*> v .:? "carbs")

    return $ FS (M.mapWithKey (namesToIngredients ingredients) foods) (Req cals prot fat carbs)
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
  
{-
parseFoodWithKey :: [Ingredient] -> T.Text -> Value -> Parser Food
parseFoodWithKey name (Array v) = do
  let ingredients = map fromJSON $ V.toList v
  return $ Food name ingredients
parseExerciseWithKey _ _ = mzero
-}
{-
loadFoodConfig :: IO FoodState
loadWeightConfig = do
  statedir <- stateDir
  fmap (maybe def id) $ decodeFile (statedir ++ "/weight.yaml")
-}

loadFoodConfig :: IO FoodState
loadFoodConfig = do
  fmap (maybe def id) $ decodeFile ("food.yaml")

