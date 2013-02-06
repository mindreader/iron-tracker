{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Food where

import Control.Lens

import Control.Monad.State
import Control.Monad.Trans (lift, liftIO)
import Control.Monad (when)

import qualified Data.Text as T hiding (find)

import Data.List (find)
import Data.Default (def)

import IO
import Menu

import Food.Types
import Food.Config

import System.Console.Haskeline (MonadException, InputT, runInputT, defaultSettings)


data FoodMenuCommand = MFInfo | MFLog | MFEat deriving (Eq, Ord)

newtype App a = App (StateT AppState IO a)
  deriving (Monad, MonadState AppState, MonadIO, Functor)

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
        MFInfo -> foodInfo
        MFLog  -> foodHistory
        MFEat  -> foodEat
  where
    menuCrud = [
      (MFInfo,  "Food Information"::T.Text),
      (MFLog,   "Recent Food History"),
      (MFEat,   "Eat Something")]


-- Get info about calorie counts in a food
foodInfo = undefined

-- Log that you ate something.
foodEat = undefined

-- Check recent food history to see what you've eaten.
foodHistory = undefined
