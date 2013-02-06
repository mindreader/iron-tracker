{-# LANGUAGE OverloadedStrings #-}

module Main where

import Util
import Menu
import Weight (runWeightRoutine)
import Food (runFoodRoutine)


import qualified Data.Text as T
import Data.Default (def)

import System.IO(stdout, stdin, hSetBuffering, BufferMode(..))


data MainMenuCommand = MMWeights | MMFood deriving (Eq, Ord)


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout LineBuffering
  createStateDir
  mainLoop


mainLoop :: IO ()
mainLoop = do
  command <- inputMenu (def { quitOption = True }) "Main Menu" $ [
      (MMWeights,       "Weight Training"::T.Text),
      (MMFood,          "Food Tracking")]

  case command of
    MenuError -> mainLoop
    MenuQuit -> return ()
    MenuInput command' -> do
      case command' of
        MMWeights -> runWeightRoutine
        MMFood    -> return () -- runFoodRoutine
