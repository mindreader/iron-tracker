module Weight.PlateOrder.Types where

import BasicPrelude


type PlateOrder = [Plate]

data Plate = P45 | P25 | P10 | P5 | P2p5 deriving (Eq, Ord, Show)
