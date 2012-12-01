{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances  #-}
module IO (
prompt, pressAnyKey,
liftIO,
module Text.Printf.Mauke
)  where 

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Safe

import System.IO (hFlush, stdout)
import System.Console.Haskeline

import Text.Printf.Mauke

instance PrintfArg TL.Text where
  embed = AStr . TL.unpack

instance PrintfArg T.Text where
  embed = AStr . T.unpack


pressAnyKey = liftIO $ printf "Press any key to continue\n" >> hFlush stdout >> getChar >> return ()

class FromString a where
  fromString :: String -> Maybe a

instance FromString Int where
  fromString = readMay

instance FromString T.Text where
  fromString = Just . T.pack

instance FromString String where
  fromString = Just

prompt :: (MonadException m, FromString a) => String -> InputT m a
prompt str = loop
  where
    loop = do
      mx <- getInputLine $ str
      case mx of
        Nothing -> loop
        Just x -> do
          case fromString x of
            Nothing -> loop
            Just x' -> return x'
