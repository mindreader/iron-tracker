{-# LANGUAGE  FlexibleInstances #-}
module IO (
prompt, pressAnyKey, printTable, pad, searchPrompt, yesnoPrompt, YNOpt(..),
liftIO,MonadException,
module Text.Printf.Mauke
)  where 

import BasicPrelude

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Safe (readMay, headMay)

import System.IO (hFlush, stdout)
import System.Console.Haskeline as Haskeline
import Control.Monad.Trans (liftIO, MonadIO)

import Text.Printf.Mauke
import Text.Regex.Posix ((=~))

instance PrintfArg TL.Text where
  embed = AStr . TL.unpack

instance PrintfArg Text where
  embed = AStr . read

pressAnyKey :: MonadIO m => m ()
pressAnyKey = liftIO $ printf "Press any key to continue\n" >> hFlush stdout >> getChar >> return ()

class FromString a where
  fromString' :: String -> Maybe a

instance FromString Int where
  fromString' = readMay

instance FromString Integer where
  fromString' = readMay

instance FromString Text where
  fromString' = Just . show

instance FromString String where
  fromString' = Just

instance FromString Float where
  fromString' = readMay

instance FromString Double where
  fromString' = readMay

instance FromString Rational where
--  fromString' = Just . toRational . read
  fromString' = readMay


data YNOpt = DefYes | DefNo
yesnoPrompt :: Text -> YNOpt -> IO Bool
yesnoPrompt str DefYes = prompt str >>= (\answer -> return $ answer /= ("no" :: Text))
yesnoPrompt str DefNo  = prompt str >>= (\answer -> return $ answer == ("yes" :: Text))

prompt :: (FromString a) => Text -> IO a
prompt str = runInputT defaultSettings $ loop
  where
    loop = do
      mx <- getInputLine $ read str
      case mx of
        Nothing -> loop
        Just x -> do
          case fromString' x of
            Nothing -> loop
            Just x' -> return x'

{-
testTable :: [[String]]
testTable = [
  [pad 12 "col1", "col2","col"],
  ["asdf",pad 123 "fdsaasdfasdf","asd"],
  ["1234567","123","000000"]]
-}

printTable :: MonadIO m => [[Text]] -> m ()
printTable tdata = do
  let inverted = transpose tdata
      colmaxlens = fmap (maximum . fmap T.length) $ inverted
  mapM_ println $ zip (cycle [colmaxlens]) tdata
  where
    println ([], _) = liftIO $ putStrLn ""
    println (_,[]) = liftIO $ putStrLn ""
    println ((len:ls),(x:xs)) = printcell len x >> println (ls, xs)
    printcell len x = liftIO $ (putStr x >> putStr (T.replicate (len - T.length x + 1) " "))

pad :: Int -> Text -> Text
pad i str = str <> T.replicate (i - T.length str) " "

searchPrompt :: MonadIO m => Text -> [Text] -> m (Maybe Text)
searchPrompt promptLabel textPossibles = do
  let -- The \t prevents it from defaulting to space, which causes it to fail on any strings with spaces in them.
      completefunc = completeWord Nothing "\t" $ return . testWords
  searchTerm <- liftIO $ runInputT (setComplete completefunc defaultSettings) $ Haskeline.getInputLine (read promptLabel)
  return $ headMay $ take 25 $ case searchTerm of
    Nothing -> textPossibles
    Just searchTerm' -> fmap show $ filter (=~ searchTerm') possibles
  where
    possibles = read <$> textPossibles
    testWords :: String -> [Completion]
    testWords left = case filter (=~ left) possibles of
      [] -> []
      [x] -> [Completion x x False]
      xs -> fmap (\str -> Completion left str False) xs
