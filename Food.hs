{-# LANGUAGE OverloadedStrings #-}
module Food where

import qualified Data.Text as T


data VolumeBase = WeightBased Int | AmountBased

type Protein = Int -- in grams
type Calories = Int -- in kcals

data Food = Food {             -- Food that is always the same (can of soup, spaghetti sauce)
  name :: T.Text,              -- For things like oreos, where multiple are eaten regularly, should ask user.
  protein :: Protein, 
  calories :: Calories,
  base :: VolumeBase
} | Recipe {                   -- A meta food composed of other foods with no modifications on its own
  name :: T.Text,
  ingredients :: [Food]
}




totalProtein :: Food -> Int
totalProtein = undefined

totalCalories :: Food -> Int
totalCalories = undefined

--totalProtein f@(FixedFood _ _ _) = protein f
--totalProtein (Ingredients _ is) = foldr (\x total -> total + protein x) 0 is

data Query = QueryWeight T.Text Int | QueryAmount T.Text

necessaryInfo :: Food -> [Query]
necessaryInfo food@(FoodFixed _ _ _ JustOne) = []
necessaryInfo food@(FoodFixed name _ _ Ask) = [QueryAmount name]
necessaryInfo food@(FoodByWeight name _ _ def) = [QueryWeight name def]
necessaryInfo (Recipe name (x:xs)) = necessaryInfo x ++ necessaryInfo (Recipe name xs)

data Reply = ReplyWeight Int | ReplyAmount Int

fillPrompts :: Food -> [Reply] -> Food
fillPrompts food@(FoodFixed name pro cals JustOne) _ = FoodFixed name pro cals JustOne
fillPrompts food@(FoodFixed name pro cals Ask) ((ReplyAmount n):_) = FoodFixed name (pro*n) (cals*n) Ask
fillPrompts food@(FoodByWeight name prof calf def) ((ReplyWeight w):xs) = FoodFixed name (prof w) (calf w) JustOne
fillPrompts _ _ = error "This shouldn't happen."


--spaghetti :: Food Incomplete
spaghetti = Recipe "Spaghetti" [hamburger, sauce, spaghetti]
  where
    hamburger = FoodByWeight "Hamburger" (\x -> round((fromIntegral x)*85/300)) (\x -> round((fromIntegral x)*690/300)) 300
    sauce = FoodFixed "Spaghetti Sauce" 20 400 JustOne
    spaghetti = FoodByWeight "Spaghetti (noodles)" (\x -> round(5/240)) (\x -> 4*x) 240
