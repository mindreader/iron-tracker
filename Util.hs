module Util(
  createStateDir, stateDir
) where

import BasicPrelude

import System.Directory (getHomeDirectory, createDirectoryIfMissing)

createStateDir = stateDir >>= createDirectoryIfMissing False
stateDir = flip mappend "/.iron-tracker" <$> getHomeDirectory

