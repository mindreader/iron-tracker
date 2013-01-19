{-# LANGUAGE OverloadedStrings #-}
module Weight.Config(
  loadWeightConfig
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Monad

import Data.Yaml

import Data.Default

import Weight.Types

instance FromJSON WeightState where
  parseJSON (Object v) = do
    -- TODO future verion of containers has M.traverseWithKey which would greatly simplify this function.
    hashmap <- v .: "exercises" >>= HM.traverseWithKey parseExerciseWithKey :: Parser (HM.HashMap T.Text Exercise)
    return $ WS $ M.fromList . HM.toList $ hashmap
  parseJSON _ = mzero

parseExerciseWithKey :: T.Text -> Value -> Parser Exercise
parseExerciseWithKey k (Object v) = do
  name       <- v .:  "name"
  minreps    <- v .:? "minreps"    .!= 4
  barbell    <- v .:? "barbell"    .!= True
  bodyweight <- v .:? "bodyweight" .!= False
  rank       <- v .:  "rank"
  return $ Exercise k name minreps barbell bodyweight rank
parseExerciseWithKey _ _ = mzero

loadWeightConfig :: IO WeightState
loadWeightConfig = fmap (maybe def id) $ decodeFile "blah.yaml"
