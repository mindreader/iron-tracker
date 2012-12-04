module Main where
import Control.Monad.Trans
import System.Console.Haskeline
import System.Console.Haskeline.Completion
import Text.Regex.Posix

import Data.List

main :: IO ()
main = do
  let completionfunc = completeWord Nothing " \t" $ return . testWords
  runInputT (setComplete completionfunc defaultSettings) $ getInputLine "word:" >>= liftIO . print


testWords :: String -> [Completion]
testWords left = case filter (=~ left) possibles of
  [] -> []
  [x] -> [Completion x x False]
  xs -> map (\str -> Completion left str False) xs
  where
    possibles = ["spaghetti","tiger" ,"sparks","silly","raft","rabid" :: String]
