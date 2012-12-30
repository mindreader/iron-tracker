{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module IO (
prompt, pressAnyKey, printTable, pad, searchPrompt, yesnoPrompt, YNOpt(..),
liftIO,MonadInput(..),
module Text.Printf.Mauke
)  where 

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Safe
import Data.List as L (transpose)

import System.IO (hFlush, stdout)
import System.Console.Haskeline as Haskeline
import System.Console.Haskeline.MonadException
import Control.Monad.State.Strict

import Text.Printf.Mauke
import Text.Regex.Posix

class MonadException m => MonadInput m where
  getInputLine :: String -> m (Maybe String)

instance MonadException m => MonadInput (InputT m) where
  getInputLine = Haskeline.getInputLine

instance (MonadInput m) => MonadInput (StateT s m) where
  getInputLine = lift . IO.getInputLine

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

instance FromString Float where
  fromString = Just . read


data YNOpt = DefYes | DefNo
yesnoPrompt :: MonadInput m => String -> YNOpt -> InputT m Bool
yesnoPrompt str DefYes = prompt str >>= (\answer -> return $ answer /= ("no" :: String))
yesnoPrompt str DefNo  = prompt str >>= (\answer -> return $ answer == ("yes" :: String))

prompt :: (MonadInput m, FromString a) => String -> m a
prompt str = loop
  where
    loop = do
      mx <- IO.getInputLine $ str
      case mx of
        Nothing -> loop
        Just x -> do
          case fromString x of
            Nothing -> loop
            Just x' -> return x'

{-
testTable :: [[String]]
testTable = [
  [pad 12 "col1", "col2","col"],
  ["asdf",pad 123 "fdsaasdfasdf","asd"],
  ["1234567","123","000000"]]
-}

printTable :: [[String]] -> IO ()
printTable tdata = do
  let inverted = L.transpose tdata
      colmaxlens = fmap (maximum . map length) $ inverted
  mapM_ println $ zip (cycle [colmaxlens]) tdata
  where
    println (_,[]) = putStrLn ""
    println ((len:ls),(x:xs)) = printcell len x >> println (ls, xs)
    printcell len x = putStr x >> putStr (replicate (len - length x + 1) ' ')

pad :: Int -> String -> String
pad i str = str ++ replicate (i - length str) ' '

searchPrompt :: MonadInput m => [T.Text] -> m [T.Text]
searchPrompt textPossibles = do
  let -- The \t prevents it from defaulting to space, which causes it to fail on any strings with spaces in them.
      completefunc = completeWord Nothing "\t" $ return . testWords
  searchTerm <- runInputT (setComplete completefunc defaultSettings) $ Haskeline.getInputLine "Search:"
  return $ take 25 $ case searchTerm of
    Nothing -> textPossibles
    Just searchTerm' -> map T.pack $ filter (=~ searchTerm') possibles
  where
    possibles = map T.unpack textPossibles
    testWords :: String -> [Completion]
    testWords left = case filter (=~ left) possibles of
      [] -> []
      [x] -> [Completion x x False]
      xs -> map (\str -> Completion left str False) xs
