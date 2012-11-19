{-# LANGUAGE OverloadedStrings #-}

module Menu(Menu(..), MenuItem(..), inputMenu, fromList) where


import Prelude hiding (catch)
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Monoid

import Data.Maybe

data MenuItem a = MenuItem {
  menuKey :: a,
  menuTitle :: T.Text
}

type MenuTitle = T.Text
data Menu a = Menu MenuTitle [MenuItem a]

fromList :: T.Text -> [(a, T.Text)] -> Menu a
fromList title list = Menu title $ map (\(x,y) -> MenuItem x y) list

displayMenu :: Menu a -> IO ()
displayMenu (Menu title items) =
  let stuff = zip [(0::Int)..] items
  in TIO.putStrLn "" >> TIO.putStrLn title >> forM_ stuff printstuff
  where
    printstuff (i, (MenuItem _ label)) = TIO.putStrLn $ mconcat [(T.pack $ show i), ". ",label]

inputMenu :: Menu a -> IO (Maybe a)
inputMenu (Menu _ []) = return Nothing
inputMenu menu@(Menu _ items) = fmap Just loop
  where loop = do
          let redo = TIO.putStrLn "Invalid input" >> loop
          i <- displayMenu menu >> fmap (listToMaybe . fmap fst . reads) getLine
          case i of
            Nothing -> redo
            Just i' -> case drop i' items of
                   [] -> TIO.putStrLn "Invalid input" >> loop
                   (x:_) -> return . menuKey $ x
