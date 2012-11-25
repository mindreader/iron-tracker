{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances  #-}
module IO (
io, prompt, pressAnyKey,
Format(..), Only(..), Shown(..), right, left, liftIO
)  where 

import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Text.Format as Fmt
import Safe

import System.IO (hFlush, stdout)


io fmt = liftIO . Fmt.print fmt

pressAnyKey = liftIO $ Fmt.print "Press any key to continue\n" () >> hFlush stdout >> getChar >> return ()

class FromString a where
  fromString :: String -> Maybe a

instance FromString Int where
  fromString = readMay

instance FromString T.Text where
  fromString = Just . T.pack

instance FromString String where
  fromString = Just

prompt fmt args = liftIO loop
  where
    loop = do
      Fmt.print fmt args
      hFlush stdout
      mx <- getLine
      case fromString mx of
        Nothing -> loop
        Just x -> return x
