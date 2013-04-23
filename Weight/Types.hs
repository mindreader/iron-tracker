{-# LANGUAGE OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, FlexibleInstances #-}
module Weight.Types where


import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Applicative
import Data.Default

data WeightState = WS {
  _exercises :: M.Map T.Text Exercise -- indexed by their id
} deriving (Show)

instance Default WeightState where
  def = WS M.empty


-- How many days should go by without doing an exercise
-- before you should just start over again at 15 reps.
type CycleLength = Integer

data ExerciseType = Bodyweight | Dumbbell | Barbell deriving (Eq, Show)

data Exercise = Exercise {
  _eExerciseId :: T.Text,  -- "bsquats"
  _eName       :: T.Text,  -- "Barbell Squats"
  _eMinReps    :: Int,     -- minimum I am willing to do
  _eType       :: ExerciseType, -- barbell, dumbbell, or bodyweight
  _eRank       :: Float    -- Determines where exercise will appear in workout
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

instance Ord Proficiency where
  compare p1 p2 = compare (p1 ^. pWeight) (p2 ^. pWeight)


