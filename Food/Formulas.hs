{-# LANGUAGE BangPatterns #-}
module Food.Formulas where

import Food.Types

import Data.Monoid (mappend)

-- cals = 9*f + 4*p + 4*c
-- c = (cals - 9*f - 4*p) / 4
--
class Scaleable a where
  scaleBy :: Float -> a -> a

instance Scaleable Nutrition where
  scaleBy factor (Nut (cals,prot,fat,carbs)) = Nut $ (
    round $ fromIntegral cals * factor,
    round $ fromIntegral prot * factor,
    round $ fromIntegral fat * factor,
    round $ fromIntegral carbs * factor)



calcCarbs :: Calories -> Protein -> Fat -> Carbs
calcCarbs cals prot fat = round $ (fromIntegral cals - (fromIntegral fat * 9 + fromIntegral prot * 4)) / 4


ingredients2Nutrition :: (Ingredient -> IO (Int,Nutrition)) -> [Ingredient] -> IO (Int, Nutrition)
ingredients2Nutrition f ingredients = do
  nutrinfo <- mapM f ingredients :: IO [(Int, Nutrition)]
  let nut = foldr mappend (Nut (0,0,0,0)) $ map snd nutrinfo
      -- If there is only one ingredient and it has a count, return that count as a number
      -- otherwise assume I ate only one of this whole recipe.
      howmany = case nutrinfo of 
                [(num,_)] -> num
                _ -> 1
  return (howmany,nut)



