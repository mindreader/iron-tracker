module Weight.Config(
  loadWeightConfig
) where

import BasicPrelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M

import Control.Monad

import Data.Yaml

import Data.Default

import Weight.Types
import Util

instance FromJSON WeightState where
  parseJSON (Object v) = do
    -- TODO future verion of containers has M.traverseWithKey which would greatly simplify this function.
    hashmap <- v .: "exercises" >>= HM.traverseWithKey parseExerciseWithKey :: Parser (HM.HashMap Text Exercise)
    return $ WS $ M.fromList . HM.toList $ hashmap
  parseJSON _ = mzero

parseExerciseWithKey :: Text -> Value -> Parser Exercise
parseExerciseWithKey k (Object v) = do
  name       <- v .:  "name"
  minreps    <- v .:? "minreps"    .!= 4
  barbell    <- v .:? "barbell"    .!= True
  bodyweight <- v .:? "bodyweight" .!= False
  dumbbell   <- v .:? "dumbbell"   .!= False
  rank       <- v .:  "rank"
  return $ Exercise k name minreps (etype barbell dumbbell bodyweight) rank
  where
    etype True _ _ = Barbell
    etype _ True _ = Dumbbell
    etype _ _ True = Bodyweight
    etype _ _ _    = Barbell

parseExerciseWithKey _ _ = mzero


loadWeightConfig :: IO WeightState
loadWeightConfig = do
  statedir <- stateDir
  fmap (maybe def id) $ decodeFile (statedir <> "/weight.yaml")
