{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Food.Log(
  logNutrition, foodLog, foodLogDay
) where

import BasicPrelude

import GHC.Float (double2Float)
import Data.Time

import Database.SQLite.Simple

import Control.Lens

import Food.Types

import Util
import Food.Formulas


withDb :: MonadIO m => (Connection -> IO a) -> m a
withDb st = do
  statedir <- liftIO $ stateDir
  liftIO $ bracket (open (statedir <> "/food.db")) close st

logNutrition :: MonadIO m => Int-> Text -> Int -> Float -> Nutrition -> m ()
logNutrition daysago name howmany howmuch (Nut (calories, protein, fat, carbs)) = withDb $ \conn -> do
    execute conn (foodInsert daysago)
      (name, howmany, howmuch, calories, protein, fat, carbs)

foodLog :: MonadIO m => Int -> m [(Text, Nutrition, Day)]
foodLog n = withDb $ \conn -> do
      rows <- query conn
        "select food, calories, protein, fat, carbs, whenit from food_log order by whenit limit ?"
        (Only n)
      return $ map (\(name,cals,prot,fat,carbs,whenit) -> (name,Nut (cals, prot, fat, carbs),whenit)) rows

foodLogDay :: MonadIO m => Int -> m [(Text, Nutrition, Int, Float)]
foodLogDay n = withDb $ \conn -> do
      rows <- query_ conn (foodQuery n)
      return $ map (\(name,howmany,howmuch,cals,prot,fat,carbs,_::Day) -> (name, Nut (cals, prot, fat, carbs), howmany, double2Float howmuch)) rows


foodInsert daysago = Query $ mconcat
  ["insert into food_log (food, howmany, howmuch, calories, protein, fat, carbs, whenit) values (?,?,?,?,?,?,?,date('now','localtime','-5 hour','-",(show daysago)," day'))"]

foodQuery daysago = Query $ mconcat
  ["select food, howmany, howmuch, calories, protein, fat, carbs, whenit from food_log where whenit = date('now','localtime','-5 hour','-", (show daysago)," day') order by whenit"]


