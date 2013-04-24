{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, FlexibleInstances #-}
module Weight.Types where

import BasicPrelude


import Control.Lens
import Data.Default

-- !A collection of exercises that are known to exist.  A person will usually have selected only a subset of these at any given time.
data WeightState = WS {
  _exercises :: Map Text Exercise -- ^ indexed by their id
} deriving (Show)

instance Default WeightState where
  def = WS empty


-- !How many days should go by without doing an exercise
--  before you should just start over again at 15 reps.
type CycleLength = Integer

data ExerciseType = Bodyweight | Dumbbell | Barbell deriving (Eq, Show)

data Exercise = Exercise {
  _eExerciseId :: Text,       -- ^ "bsquats"
  _eName       :: Text,       -- ^ "Barbell Squats"
  _eMinReps    :: Int,          -- ^ minimum I am willing to do
  _eType       :: ExerciseType,
  _eRank       :: Float         -- ^ Determines where exercise will appear in workout (TODO get rid of this hack)
} deriving (Eq, Show)



type Weight = Rational
type Reps   = Int
data Proficiency = Pro {
  _pReps       :: Reps,
  _pWeight     :: Weight
} deriving (Eq, Show)


makeLenses ''WeightState
makeLenses ''Exercise
makeLenses ''Proficiency

instance Ord Exercise where
  compare e1 e2 = compare (e1 ^. eRank) (e2 ^. eRank)
