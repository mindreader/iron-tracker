{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, BangPatterns #-}
module Food where

import BasicPrelude

import Control.Lens


import Control.Monad.State
import Control.Monad.Trans (lift, liftIO)

import qualified Data.Text as T hiding (find)
import qualified Data.Text.IO as TIO (putStrLn)

import Data.List (find, sortBy)
import Data.Function (on)
import qualified Data.Map as M
import Data.Default (def)
import Data.Maybe (isJust)

import IO
import Menu

import Safe

import System.Console.Haskeline (MonadException, InputT, runInputT, defaultSettings)

import Food.Types
import Food.Config
import Food.Formulas
import Food.Log




data FoodMenuCommand = MFStatsToday | MFStatsYesterday | MFEatToday | MFEatYesterday | MFInfo | MFLog  deriving (Eq, Ord)

newtype App a = App (StateT AppState IO a)
  deriving (Monad, MonadState AppState, MonadIO, Functor, Applicative)

data AppState = AS {
  _foodState:: FoodState
}

makeLenses ''AppState

runFoodRoutine :: IO ()
runFoodRoutine = runApp mainLoop

runApp :: App a -> IO a
runApp (App s) = do
  fs <- loadFoodConfig
  evalStateT s (AS fs )




mainLoop :: App ()
mainLoop = do
  command <- inputMenu (def { quitOption = True }) "Food Menu" menuCrud
  case command of
    MenuError -> mainLoop
    MenuQuit -> return () 
    MenuInput command' -> do
      case command' of
        MFInfo           -> foodInfo >> return ()
        MFLog            -> undefined -- foodHistory
        MFStatsToday     -> foodStatsToday
        MFStatsYesterday -> foodStatsYesterday
        MFEatToday       -> foodEatToday
        MFEatYesterday   -> foodEatYesterday
      mainLoop
  where
    menuCrud = [
      (MFStatsToday,     "Today's Statistics"),
      (MFStatsYesterday, "Yesterdays's Statistics"),
      (MFInfo,           "Food Information"::T.Text),
      (MFLog,            "Recent Food History"),
      (MFEatToday,       "Eat Something"),
      (MFEatYesterday,   "Eat Something Yesterday")]


-- Get info about calorie counts in a food
foodInfo :: App (Maybe (T.Text, Int, Float, Nutrition))
foodInfo = do
  foods <- use (foodState . foods)
  mfood <- liftIO $ searchPrompt "Food Search:" $ (sortBy (compare `on` T.length) . fmap (view fName) . M.elems) foods
  case mfood of
    Just foodName -> case M.lookup foodName foods of
      Just food@(Food name ingredients) -> do
        (howmany,nutrition) <- liftIO $ ingredients2Nutrition queryIngredient ingredients
        percent <- if any (\ing -> isJust $ ing ^. iServingNumber) ingredients
          then return 1.0
          else (/ 100) . fromIntegral <$> (liftIO (prompt . show $ (printf "What percentage of this meal did you just eat? (100):" :: String) :: IO Int))
        showFood food $ scaleBy percent nutrition
        return $ Just $ (name, howmany, percent, scaleBy percent $ nutrition)
      Nothing -> return Nothing
    Nothing -> return Nothing

showFood :: MonadIO m => Food -> Nutrition -> m ()
showFood (Food name _) (Nut (cals, prot, fat, carbs)) = liftIO $ do
  printf " %s:\n" name
  printf "  Calories : %d\n" cals
  printf "  Protein  : %d\n" prot
  printf "  Fat      : %d\n" fat
  printf "  Carbs    : %d\n" carbs

queryIngredient :: Ingredient -> IO (Int, Nutrition)
queryIngredient (Ing name sSize sNum cals prot fat carbs) = do

    scale <- case sSize of
      Just sSize' -> do
        grams <- (prompt . show $ (printf "How many grams of %s? (%d):" name sSize' :: String)) :: IO Int
        return $ (fromIntegral grams) / (fromIntegral sSize')
      Nothing -> return (1.0 :: Float)

    multiply <- case sNum of
      Just sNum' -> prompt . show $ (printf "How many %s did you eat? (%d):" name sNum' :: String) :: IO Int
      Nothing -> return (1 :: Int)

    return $ (multiply, scaleBy scale . scaleBy (fromIntegral multiply) $ Nut (cals, prot, fat, carbs))

foodEatYesterday :: App ()
foodEatYesterday = foodEatWhenever 1
foodEatToday :: App()
foodEatToday = foodEatWhenever 0

-- Log that you ate something.
foodEatWhenever :: Int -> App ()
foodEatWhenever daysago = do
  mfood <- foodInfo
  case mfood of
    Just (name, howmany, howmuch, nutrition) -> logNutrition daysago name howmany howmuch nutrition
    Nothing -> return ()


-- Check recent food history to see what you've eaten.
foodStatsToday :: App()
foodStatsToday = foodStatsWhenever 0

foodStatsYesterday :: App()
foodStatsYesterday = foodStatsWhenever 1


foodStatsWhenever :: Int -> App ()
foodStatsWhenever daysago = do
  hist <- foodLogDay daysago :: App [(T.Text, Nutrition, Int, Float)]
  let nuts      = fmap (view _2) hist :: [Nutrition]
      (Nut (cals,prot,fat,carbs)) = foldr mappend mempty nuts
  liftIO $ do
    printf "Eaten today:\n"
    when (length hist == 0) $ printf " Nothing!\n"
    mapM_ showRow hist
    printf "\nTotals:\n"

    printf " Calories : %d\n" cals
    printf " Protein  : %d\n" prot
    printf " Fat      : %d\n" fat
    printf " Carbs    : %d\n" carbs

  where
    showRow :: (T.Text, Nutrition, Int, Float) -> IO ()
    showRow (name,_,howmany,howmuch) | howmany == 1 && howmuch == 1.0 = printf " %s\n" name
                                     | howmany /= 1 && howmuch == 1.0 = printf " %s (%d)\n" name howmany
                                     | howmany == 1 && howmuch /= 1.0 = printf " %s (%d%%)\n" name (floor $ howmuch * 100 :: Int)
                                     | otherwise {- howmuch /= 1.0 -} = printf " %s (%d) (%d%%)\n" name howmany (floor $ howmuch * 100 :: Int)
