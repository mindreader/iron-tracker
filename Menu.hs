{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies #-}

module Menu where


import Prelude hiding (catch)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Maybe
import qualified Data.Map as M
import Safe (atMay, readMay)
import Data.Default

import Control.Monad (when)

import Data.Char (isAlphaNum)
import Data.List (findIndex)

import IO

class Menuable a where
  type MenuKey a
  toMenu :: a -> Menu (MenuKey a)


instance Menuable [T.Text] where
  type MenuKey [T.Text] = T.Text
  toMenu = toMenu .  map (\x -> (x,x))

instance (Ord a) => Menuable [(a, T.Text)] where
  type MenuKey [(a, T.Text)] = a
  toMenu = Menu . M.mapWithKey (\k v -> MenuItem k v) . M.fromList




type MenuTitle = T.Text
data Menu a = Menu {
  menuItems :: M.Map a (MenuItem a)
} deriving Show

data MenuItem a = MenuItem {
  menuKey :: a,
  menuTitle :: T.Text
} deriving Show


data MenuOptions = MenuOptions {
  quitOption :: Bool
}

instance Default MenuOptions where
  def = MenuOptions False

data MenuResult b = MenuQuit | MenuInput b | MenuError

inputMenu :: Menuable a => MenuOptions -> T.Text -> a -> IO (MenuResult (MenuKey a))
inputMenu opts title menuable = loop
  where
    loop = case items of
      [] -> return MenuError
      items' -> do
        display title items'
        when (quitOption opts) $ printf "q. Quit\n"
        key <- getChar
        printf "\n"
        if ((quitOption opts && (key == 'q')) || (not $ isJust $ findIndex (==key) inputChars))
          then return MenuQuit
          else maybe loop (return . MenuInput . menuKey) $ items' `atMay` (maybe 0 id (findIndex (==key) inputChars))

    items = (M.elems . menuItems . toMenu $ menuable)

    display :: T.Text -> [MenuItem a] -> IO ()
    display title items = do
      printf "\n%s\n" title
      mapM_ printLine (zip inputChars items)

    printLine (i, (MenuItem _ label)) = printf "%c. %s\n" i label
    inputChars = filter (/= 'q') $ filter isAlphaNum $ ['1'..'9'] ++ ['0'] ++ ['a'..'z'] ++ ['A'..'Z']
