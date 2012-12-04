{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad (when)
import qualified Data.Text as T
import Menu
import Data.Default (def)

import IO
import FoodState

import System.Console.Haskeline (MonadException, InputT, runInputT, defaultSettings)


data FoodMenuCommand = MFAddFood | MFRemFood | MFAddRecipe | MFInfo deriving (Eq, Ord)

runFoodRoutine :: MonadException m => m ()
runFoodRoutine = runInputT defaultSettings (runFoodStateT mainLoop)




mainLoop :: (MonadException m) => FoodStateT (InputT m) ()
mainLoop = do
  command <- liftIO $ inputMenu (def { quitOption = True }) "Food Menu"
    [(MFInfo,      "Food Information"::T.Text),
     (MFAddFood,   "Add Ingredient"),
     (MFRemFood,   "Delete Ingredient"),
     (MFAddRecipe, "Create Recipe")]

  case command :: MenuResult FoodMenuCommand of
    MenuError -> mainLoop
    MenuQuit -> return () 
    MenuInput command' -> do
      case command' of      
        MFInfo      -> foodInfo
        MFAddFood   -> createFood
        MFRemFood   -> deleteFood
        MFAddRecipe -> createRecipe


createFood :: MonadException m => FoodStateT (InputT m) () 
createFood = undefined

deleteFood :: MonadException m => FoodStateT (InputT m) () 
deleteFood = undefined

createRecipe :: MonadIO m => FoodStateT m () 
createRecipe = undefined

foodInfo :: MonadException m => FoodStateT (InputT m) ()
foodInfo = do
  foods <- foodList
