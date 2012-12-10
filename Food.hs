{-# LANGUAGE OverloadedStrings #-}
module Food where


import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad (when)
import qualified Data.Text as T hiding (find)
import Menu
import Data.Default (def)
import Data.List (find)

import IO
import FoodState

import System.Console.Haskeline (MonadException, InputT, runInputT, defaultSettings)


data FoodMenuCommand = MFSearch | MFAddFood | MFRemFood | MFAddRecipe deriving (Eq, Ord)

runFoodRoutine :: MonadException m => m ()
runFoodRoutine = runInputT defaultSettings (runFoodStateT mainLoop)


mainLoop :: (MonadException m) => FoodStateT (InputT m) ()
mainLoop = do
  command <- liftIO $ inputMenu (def { quitOption = True }) "Food Menu"
    [(MFSearch,    "Food Information"::T.Text),
     (MFAddFood,   "Add Ingredient"),
     (MFRemFood,   "Delete Ingredient"),
     (MFAddRecipe, "Create Recipe")]

  case command :: MenuResult FoodMenuCommand of
    MenuError -> mainLoop
    MenuQuit -> return () 
    MenuInput command' -> do
      case command' of
        MFSearch    -> foodSearch
        MFAddFood   -> createFood
        MFRemFood   -> deleteFood
        MFAddRecipe -> createRecipe


createFood :: MonadException m => FoodStateT (InputT m) ()
createFood = do
  name          <- lift $ prompt "Name:"
  protein       <- lift $ prompt "Protein:"
  fat           <- lift $ prompt "Fat:"
  carbs         <- lift $ prompt "Carbs:"
  askWeight     <- lift $ yesnoPrompt "Weight of food varies meal to meal? (no):" DefNo
  weight <- if (askWeight)
              then fmap Just . lift $ prompt "Serving size (weight):"
              else return Nothing
  askAmount     <- lift $ yesnoPrompt "Number of food items varies per meal? (no):" DefNo

  addFood $ Food name protein fat carbs weight askAmount
  

deleteFood :: MonadException m => FoodStateT (InputT m) ()
deleteFood = do
  foods <- fmap (map name) foodList >>= lift . searchPrompt
  mfood <- liftIO $ inputMenu (def { quitOption = True }) "Pick A Food" $ take 25 $ foods
  case mfood of
    MenuInput food -> remFood food
    otherwise -> return ()

createRecipe :: MonadIO m => FoodStateT m ()
createRecipe = undefined

foodSearch :: MonadException m => FoodStateT (InputT m) ()
foodSearch = do
  foods <- foodList
  foodNamesFiltered <- lift . searchPrompt $ map name foods  -- TODO make a SearchPromptable class similar to Menuable
  let foodsFiltered = filter (\food -> name food `elem` foodNamesFiltered) foods

  mfood <- liftIO $ inputMenu (def { quitOption = True }) "Pick A Food" $ take 25 $ map (\food -> (food, name food)) foodsFiltered
  case mfood of
    MenuInput food -> do
      multiplier <- case (weightBased food) of
                      Nothing     -> return 1              -- TODO Get rid of this ugly hack as soon as possible.
                      Just weight -> fmap (\x -> case weight of 0 -> 1; weight' -> x / (fromIntegral weight') :: Float) $ lift $ prompt "How many grams of food?"
      liftIO $ printf "Name:%s\n  Calories:%.1f\n  Protein:%.1f\n  Fat:%.1f\n  Carbohydrates:%.1f\n"
          (name food)
          (fromIntegral (calories food) * multiplier)
          (fromIntegral (protein food)  * multiplier)
          (fromIntegral (fat food)      * multiplier)
          (fromIntegral (carbs food)    * multiplier)
    otherwise -> return ()

  where
