{-# LANGUAGE OverloadedStrings #-}
module Weight.Config(
  loadWeightConfig
) where

--import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
--import qualified Data.Map as M

-- import Control.Monad

import Data.Yaml

import Data.Default

import Food.Types

instance FromJSON FoodState where
  parseJSON (Object v) = do
    ingredients <- v .: "ingredients"
    foods <- v .: "foods" >>= HM.traverseWithKey (parseFoodWithKey ingredients) :: Parser (HM.HashMap T.Text Food)
    -- TODO future verion of containers has M.traverseWithKey which would greatly simplify this function.
--    hashmap <- v .: "exercises" >>= HM.traverseWithKey parseExerciseWithKey :: Parser (HM.HashMap T.Text Exercise)
--    return $ WS $ M.fromList . HM.toList $ hashmap
  parseJSON _ = mzero

instance FromJSON Food where
  parseJSON (Object v) = undefined
  parseJSON _ = mzero

instance FromJSON Ingredient where
  parseJSON (Object v) = undefined
  parseJSON _ = mzero
  

parseFoodWithKey :: [Ingredient] -> T.Text -> Value -> Parser Food
parseFoodWithKey name (Array v) = do
  let ingredients = map fromJSON $ V.toList v
  return $ Food name ingredients
parseExerciseWithKey _ _ = mzero

loadFoodConfig :: IO FoodState
loadWeightConfig = do
  statedir <- stateDir
  fmap (maybe def id) $ decodeFile (statedir ++ "/weight.yaml")
