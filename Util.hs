module Util(
  createStateDir, stateDir
) where

import System.Directory (getHomeDirectory, createDirectoryIfMissing)


createStateDir = do
  dir <- stateDir
  createDirectoryIfMissing False dir
  
stateDir = do
  statedir <- fmap (++ "/.iron-tracker") $ getHomeDirectory
  return $ statedir

