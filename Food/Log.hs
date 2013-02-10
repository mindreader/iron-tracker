{-# LANGUAGE OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Food.Log(
  logNutrition, foodLog, foodLogToday
) where

import GHC.Float (double2Float)
import Data.Time

import Database.SQLite.Simple


import qualified Data.Text as T

import Control.Applicative
import Control.Lens
import Control.Exception (bracket)

import Food.Types
import Control.Monad.Trans (liftIO, MonadIO)

import Util
import Food.Formulas


withDb :: MonadIO m => (Connection -> IO a) -> m a
withDb st = do
  statedir <- liftIO $ stateDir
  liftIO $ bracket (open (statedir++"/food.db")) close st

logNutrition :: MonadIO m => T.Text -> Int -> Float -> Nutrition -> m ()
logNutrition name howmany howmuch (Nut (calories, protein, fat, carbs)) = withDb $ \conn -> do
    execute conn
      "insert into food_log (food, howmany, howmuch, calories, protein, fat, carbs, whenit) values (?,?,?,?,?,date('now','localtime','-5 hour'))"
      (name, howmany, howmuch, calories, protein, fat, carbs)

foodLog :: MonadIO m => Int -> m [(T.Text, Nutrition, Day)]
foodLog n = withDb $ \conn -> do
      rows <- query conn
        "select food, calories, protein, fat, carbs, whenit from food_log order by whenit limit ?"
        (Only n)
      return $ map (\(name,cals,prot,fat,carbs,whenit) -> (name,Nut (cals, prot, fat, carbs),whenit)) rows

foodLogToday :: MonadIO m => m [(T.Text, Nutrition, Int, Float)]
foodLogToday = withDb $ \conn -> do
      rows <- query_ conn "select food, howmany, howmuch, calories, protein, fat, carbs, whenit from food_log where whenit = date('now','localtime','-5 hour') order by whenit"
      return $ map (\(name,howmany,howmuch,cals,prot,fat,carbs,_::Day) -> (name, Nut (cals, prot, fat, carbs), howmany, double2Float howmuch)) rows