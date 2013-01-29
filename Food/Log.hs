{-# LANGUAGE OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Food.Log(
  logFood, foodHistory
) where

import Data.Time

import Database.SQLite.Simple


import Control.Applicative
import Control.Lens
import Control.Exception (bracket)

import Food.Types
import Control.Monad.Trans (liftIO, MonadIO)

import Util

withDb :: MonadIO m => (Connection -> IO a) -> m a
withDb st = do
  statedir <- liftIO $ stateDir
  liftIO $ bracket (open (statedir++"/food.db")) close st

logFood :: MonadIO m => (Calories, Protein, Fat) -> m ()
logFood (calories, protein, fat) = withDb $ \conn -> do
    execute conn
      "insert into food_log (calories, protein, fat, whenit) values (?,?,?,date('now','localtime','-5 hour'))"
      (calories, protein, fat)

foodHistory :: MonadIO m => (Calories, Protein, Fat) -> Int -> m [(Calories, Protein, Fat, Day)]
foodHistory exer n = withDb $ \conn -> query conn
        "select calories, protein, fat, whenit from food_log where whenit date('now','localtime','-? day') order by whenit"
        (Only n)
