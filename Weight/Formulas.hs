module Weight.Formulas(
  TryThis(..), DidThis(..), suggestNewRepWeight, adjustProfByReps
) where


import Data.Time

import Weight.Types

data TryThis = TryThis Reps Weight deriving Show

-- DidThis entries are sorted newest to oldest
data DidThis  = DidThis {
  toldreps :: Reps,
  reps :: Reps,
  weight :: Weight,
  day :: Day
} deriving Show

data Progress = Stagnant | Improved | Regressed


adjustProfByReps :: Reps -> Proficiency -> Proficiency
adjustProfByReps r' (Pro r w) = (Pro r' (changeReps r' r w))

suggestNewRepWeight :: Integer -> Day -> [DidThis] -> TryThis
suggestNewRepWeight cycleLength today didthis = case removeOldCycles today didthis of
    [] ->      nextWeightBeginAgain didthis
    cycles ->  nextWeight' cycles
  where

    --Restarting at beginning, if we have past history, then use it.
    nextWeightBeginAgain :: [DidThis] -> TryThis
    nextWeightBeginAgain [] = TryThis 15 0
    nextWeightBeginAgain (x:xs) = TryThis 15 $ changeReps 15 (reps x) (weight x)

    --Continuing on weight progression
    nextWeight' :: [DidThis] -> TryThis
    nextWeight' logs = case logs of


      -- Regardless of anything, if the weight hasn't changed upward in five workouts, weight needs to change.
      -- Go down two reps because low weight exercises often one rep isn't enough.
      all@(x:xs) | length all >= 5 && sameorlessweight all ->
        let newreps = toldreps x - if toldreps x > 10 then 2 else 1 
        in {- trace "sameweight" $ -} TryThis newreps $ changeReps newreps (reps x) (weight x)

      -- Improved over last workout, renormalize at same reps as you were originally told to do.
      (x:_) | improved x ->
        {- trace "improved" $ -} TryThis (toldreps x) $ changeReps (toldreps x) (reps x) (weight x)

      -- If you are above 15, regardless of previous workout, readjust down to 15.
      -- Can happen on first workout, if you severely underestimate your strength.
      (x:_) | reps x > 15 ->
        {- trace "above 15" $ -} TryThis 15 $ changeReps 15 (reps x) (weight x)

      -- Stagnant over 3 workouts, renormalize at lower rep count
      (x:y:z:_) | stagnant x y && stagnant y z ->
        {- trace "stagnant over 3" $ -} TryThis (toldreps x - 1) $ changeReps (toldreps x - 1) (reps x) (weight x)

      -- Reps have declined and stayed down for two workouts.  Renormalize at lower weight at original rep count.
      (x:y:_) | declined x && declined y ->
        {- trace "decline over 2" $ -} TryThis (toldreps x) $ changeReps (toldreps x) (reps x) (weight x)

      -- Stagnant or declined over 2 workouts, just repeat workout
      (x:y:_) | (stagnant x y || declined x) && (stagnant x y || declined y) -> 
        {- trace "stagnant or declined over 2" $ -} TryThis (toldreps x) (weight x)

      -- We don't have enough history to determine next workout.  Just repeat what you just did.
      -- Also, if there are any holes in the above logic, we'll end up here.  Hopefully appropriate.
      (x:_) -> {- trace "not enough history" $ -} TryThis (toldreps x) (weight x)

      -- You've never done this before, you figure out what to do, and then tell me how you did for next time.
      [] -> {- trace "never done before" $ -} TryThis 15 0

    --If the new weight is the same as the old weight, give back the original rep count instead and keep weight the same
  {-  adjustWeight r' r w = if w' == w
      then (r,w)
      else (r',w')
      where w' = changeReps r' r w -}

    -- If a workout happened 14 days or more ago, it might as well not have happened as far as the
    -- above rules are concerned.  This happens when you stop doing an exercise for awhile, or fall
    -- off the wagon entirely for a time.  Might as well start from scratch in that case.
    removeOldCycles :: Day -> [DidThis] -> [DidThis]
    removeOldCycles today logs = case logs of
        [] -> []
        (x:_) | today > addDays cycleLength (day x) -> []
        (x:xs) -> x : removeOldCycles (day x) xs

improved, declined ::  DidThis -> Bool
improved x = reps x > toldreps x
declined x = reps x < toldreps x
stagnant :: DidThis -> DidThis -> Bool
stagnant x y = toldreps x == toldreps y && reps x <= reps y && weight x <= weight y
sameorlessweight xs = maximum weights <= last weights
  where  weights = map weight xs

  
test2 = do
  today <- fmap (localDay . zonedTimeToLocalTime) getZonedTime
--  c <- fmap (map (\(day, (Pro r w)) -> (DidThis r w day))) $ liftHistory (Exercise "bsquats" undefined undefined undefined undefined undefined) 20
  let d = [
            DidThis 11 11 70 today,
            DidThis 11 11 70 (addDays (-1) today),
            DidThis 11 11 70 (addDays (-2) today),
            DidThis 11 11 72 (addDays (-3) today)]
  let c= drop 1 d
  let b= drop 1 c
  let a = drop 1 b
  print $ a
  print $ suggestNewRepWeight 14 today a
  print $ b
  print $ suggestNewRepWeight 14 today b
  print $ c
  print $ suggestNewRepWeight 14 today c
  print $ d
  print $ suggestNewRepWeight 14 today d
{-
test = do
  today <- fmap (localDay . zonedTimeToLocalTime) getZonedTime
  let a = [DidThis 12 160 (addDays (-5) today)] --, DidThis 8 150 (addDays (-5) today), DidThis 8 150 (addDays (-7) today)]
  let b = [DidThis 15 160 (addDays (-4) today), DidThis 12 160 (addDays (-5) today)] --, DidThis 8 150 (addDays (-5) today), DidThis 8 150 (addDays (-7) today)]
  let c = [DidThis 14 170 (addDays (-3) today) ,DidThis 15 160 (addDays (-4) today), DidThis 12 160 (addDays (-5) today)] --, DidThis 8 150 (addDays (-5) today), DidThis 8 150 (addDays (-7) today)]
  print $ head a
  print $ suggestNewRepWeight today a
  print $ head b
  print $ suggestNewRepWeight today b
  print $head c
  print $ suggestNewRepWeight today c
  return ()
 -- print $ removeOldCycles today a
-}
-- changeReps newreps r w = epleyToNearest5 r w newreps
changeReps :: Reps -> Reps -> Weight -> Weight
changeReps newreps r w = nearest 5 $ epley r w newreps
  where nearest x = (*x) . toRational . round . (/x)


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


epley :: Reps -> Weight -> Reps -> Weight
epley r w r' = w * ((toRational r) + 30) / ((toRational r') + 30)

epleyCompare :: Reps -> Weight -> Reps -> Weight -> Ordering
epleyCompare r1 w1 r2 w2 = compare (epley r1 w1 1) (epley r2 w2 1)

--epleyToNearestPoint5 = epley $ (/2) . fromIntegral . round . (*2)
