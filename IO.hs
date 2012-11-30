{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances  #-}
module IO (
io, prompt, pressAnyKey,
Format(..), Only(..), Shown(..), right, left, liftIO
)  where 

import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Format as Fmt
import Safe

import System.IO (hFlush, stdout)
import System.Console.Haskeline


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

prompt fmt args = loop
  where
    loop = do
      mx <- lift . getInputLine . TL.unpack $ Fmt.format fmt args
      case mx of
        Nothing -> loop
        Just x -> do
          case fromString x of
            Nothing -> loop
            Just x' -> return x'
