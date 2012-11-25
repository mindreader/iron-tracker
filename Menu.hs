{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

module Menu where


import Prelude hiding (catch)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Format as Fmt

import Data.Maybe
import qualified Data.Map as M
import Safe (atMay, readMay)
import Data.Default

import Control.Monad (when)

import Data.Char (isAlphaNum)
import Data.List (findIndex)


data MenuOptions = MenuOptions {
  quitOption :: Bool
}

instance Default MenuOptions where
  def = MenuOptions False


class Menuable a b | b -> a where
  toMenu :: a -> Menu b


--instance Menuable [T.Text] T.Text where
--  toMenu = Menu . M.map (\v -> MenuItem v v) . M.fromList . map (\x -> (x,x))

instance (Ord a) => Menuable [(a, T.Text)] a where
  toMenu = Menu . M.mapWithKey (\k v -> MenuItem k v) . M.fromList



type MenuTitle = T.Text
data Menu a = Menu {
  menuItems :: M.Map a (MenuItem a)
}
data MenuItem a = MenuItem {
  menuKey :: a,
  menuTitle :: T.Text
}

data MenuResult b = MenuQuit | MenuInput b | MenuError

inputMenu :: Menuable a b => MenuOptions -> T.Text -> a -> IO (MenuResult b)
inputMenu opts title menuable = loop
  where
    loop = case items of
      [] -> return MenuError
      items' -> do
        display title items'
        when (quitOption opts) $ Fmt.print "q. Quit\n" ()
        key <- getChar
        Fmt.print "\n" ()
        if ((quitOption opts && (key == 'q')) || (not $ isJust $ findIndex (==key) inputChars))
          then return MenuQuit
          else maybe loop (return . MenuInput . menuKey) $ items' `atMay` (maybe 0 id (findIndex (==key) inputChars))

    items = (M.elems . menuItems . toMenu $ menuable)

    display :: T.Text -> [MenuItem a] -> IO ()
    display title items = do
      Fmt.print "\n{}\n" (Only title)
      mapM_ printLine (zip inputChars items)

    printLine (i, (MenuItem _ label)) = Fmt.print "{}. {}\n" (i,label)
    inputChars = filter (/= 'q') $ filter isAlphaNum $ ['1'..'9'] ++ ['0'] ++ ['a'..'z'] ++ ['A'..'Z']
