module Weight.Formulas(
  TryThis(..), DidThis(..), suggestNewRepWeight, adjustProfByReps
) where


{-
 - http://en.wikipedia.org/wiki/One-repetition_maximum#Epley_Formula
 -
 - 1 = (wr / 30) + w
 - (w'r' / 30) + w'          = (wr / 30) + w
 - (w'r' / 30) + (30w' / 30) = ...
 - (w'r' + 30w') /  30       = ...
 - w'r' + 30w'               = 30 (wr / 30 + w)
 - w' * (r' + 30)            = 30 (wr / 30 + w)
 - ...                       = wr + 30w
 - w'                        = (wr + 30w) / (r' + 30)
 - w'                        = w(r + 30) / (r' + 30)
 -}

import Data.Time
import Data.List (dropWhileEnd)

import Weight.Types

data TryThis = TryThis Reps Weight deriving Show

-- DidThis entries are sorted newest to oldest
data DidThis  = DidThis {
  reps :: Reps,
  weight :: Weight,
  day :: Day
} deriving Show


adjustProfByReps :: Reps -> Proficiency -> Proficiency
adjustProfByReps r' (Pro r w) = (Pro r' (changeReps r' r w))

suggestNewRepWeight :: Day -> [DidThis] -> TryThis
suggestNewRepWeight today = nextWeight' . removeOldCycles today
  where
    nextWeight' :: [DidThis] -> TryThis
    nextWeight' logs = case logs of

      -- Stagnant over 3 workouts, renormalize at lower rep count
      (x:y:z:_) | reps x <= reps y && reps y <= reps z ->
        TryThis r' w' where (r',w') = adjustWeight (reps z - 1) (reps z) (weight z)

      -- Reps have declined and stayed down for two workouts.  Renormalize at lower weight at original rep count.
      (x:y:z:_) | reps x < reps z && reps y < reps z ->
        TryThis r' w' where (r',w') = adjustWeight (reps z) (reps x) (weight z)

      -- Stagnant over 2 workouts (possibly with decline), repeat original workout
      (x:y:_)   | reps x <= reps y -> 
        TryThis (reps y) (weight y)

      -- Improved over last workout, renormalize at same reps.  Go back to find reps you started with at this weight.
      (x:y:xs)   | reps x > reps lastAtXWeight ->
        TryThis r' w'
          where
            lastAtXWeight = last (dropWhileEnd (\n -> weight n /= weight x) (y:xs))
            (r',w') = adjustWeight (reps lastAtXWeight) (reps x) (weight x)

      -- If you are above 15, regardless of previous workout, readjust down to 15.
      -- Can happen on first workout, if you severely underestimate your strength.
      (x:_)    | reps x > 15 ->
        TryThis 15 $ changeReps 15 (reps x) (weight x)

      -- We don't have enough history to determine next workout.  Just repeat what you just did.
      -- Also, if there are any holes in the above logic, we'll end up here.  Hopefully appropriate.
      (x:_) -> TryThis (reps x) (weight x)

      -- You've never done this before, you figure out what to do, and then tell me how you did for next time.
      [] -> TryThis 15 0

    --If the new weight is the same as the old weight, give back the original rep count instead and keep weight the same
    adjustWeight r' r w = if w' == w
      then (r,w)
      else (r',w')
      where w' = changeReps r' r w

    -- If a workout happened 14 days or more ago, it might as well not have happened as far as the
    -- above rules are concerned.  This happens when you stop doing an exercise for awhile, or fall
    -- off the wagon entirely for a time.  Might as well start from scratch in that case.
    removeOldCycles :: Day -> [DidThis] -> [DidThis]
    removeOldCycles today logs = case logs of
        [] -> []
        (x:_) | today > addDays 14 (day x) -> []
        (x:xs) -> x : removeOldCycles (day x) xs

{-
test = do
  today <- fmap (localDay . zonedTimeToLocalTime) getZonedTime
  let a = [DidThis 20 150 (addDays (-3) today)] -- ,DidThis 6 150 (addDays (-4) today)] --, DidThis 8 150 (addDays (-5) today), DidThis 8 150 (addDays (-7) today)]
  print $ a
  print $ suggestNewRepWeight today a
  return ()
 -- print $ removeOldCycles today a
-}

-- changeReps newreps r w = epleyToNearest5 r w newreps
changeReps :: Reps -> Reps -> Weight -> Weight
changeReps newreps r w = nearest 5 $ epley r w newreps
  where nearest x = (*x) . toRational . round . (/x)

epley :: Reps -> Weight -> Reps -> Weight
epley r w r' = w * ((toRational r) + 30) / ((toRational r') + 30)

--epleyToNearestPoint5 = epley $ (/2) . fromIntegral . round . (*2)
