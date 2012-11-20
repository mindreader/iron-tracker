{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

module Menu where


import Prelude hiding (catch)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Format as Fmt

import Data.Maybe
import qualified Data.Map as M
import Safe (atMay)


class Menuable a b | b -> a where
  toMenu :: a -> Menu b

instance (Ord a) => Menuable [(a, T.Text)] a where
  toMenu list = Menu $ M.mapWithKey (\x y -> MenuItem x y) $ M.fromList $ list


type MenuTitle = T.Text
data Menu a = Menu {
  menuItems :: M.Map a (MenuItem a)
}
data MenuItem a = MenuItem {
  menuKey :: a,
  menuTitle :: T.Text
}


inputMenu :: Menuable a b => T.Text -> a -> IO (Maybe b)
inputMenu title menuable = loop
  where
    loop = case items of
      [] -> return Nothing
      items' -> do
        display title items'
        mkey <- fmap (listToMaybe . fmap fst . reads) getLine
        case mkey of
          Nothing -> loop
          Just key -> maybe loop (return . Just . menuKey) $ items' `atMay` key

    items = (M.elems . menuItems . toMenu $ menuable)

    display :: T.Text -> [MenuItem a] -> IO ()
    display title items = do
      Fmt.print "\n{}\n" (Only title)
      mapM_ printLine (zip [(0::Int)..] items)

    printLine (i, (MenuItem _ label)) = Fmt.print "{}. {}\n" (i,label)
