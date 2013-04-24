module Weight.PlateCalc(displayPlateCalc,Plate(..),plateCalc, Plates(Plates,getPlates), BarType(..)) where

import BasicPrelude


newtype Plates = Plates {
  getPlates :: [Plate]
}

--instance Show Plates where
--  show = displayPlates

data BarType = Barbell | Dumbbell deriving (Show)

data Plate = P45 Int | P25 Int | P10 Int | P5 Int | P2p5 Int | TooLight deriving Show

displayPlateCalc :: BarType -> Rational -> Text
displayPlateCalc ctype = displayPlates . plateCalc ctype


displayPlates :: Plates -> Text
displayPlates (Plates []) = "just the bar"
displayPlates (Plates ps) = dPlates ps
  where
    dPlates :: [Plate] -> Text
    dPlates = concat . intersperse "," . map dPlate

    dPlate :: Plate -> Text
    dPlate (TooLight) = "less than a bar"
    dPlate (P45  n) = "45x"  <> show n
    dPlate (P25  n) = "25x"  <> show n
    dPlate (P10  n) = "10x"  <> show n
    dPlate (P5   n) = "5x"   <> show n
    dPlate (P2p5 n) = "2.5x" <> show n



plateCalc :: BarType -> Rational -> Plates
plateCalc ctype lbs | lbs < barWeight ctype = Plates [TooLight]
                      | otherwise = Plates $ minimize $ plateCalc' (lbs - barWeight ctype)

  where
    barWeight Barbell  = 45
    barWeight Dumbbell = 2.5

    plateCalc' :: Rational -> [Plate]
    plateCalc' lbs | lbs >= 90  = P45  2:plateCalc' (lbs - 90)
                   | lbs >= 50  = P25  2:plateCalc' (lbs - 50)
                   | lbs >= 20  = P10  2:plateCalc' (lbs - 20)
                   | lbs >= 10  = P5   2:plateCalc' (lbs - 10)
                   | lbs >= 5   = P2p5 2:plateCalc' (lbs - 5)
                   | lbs >= 2.5 = P2p5 2:plateCalc' (lbs - 5) -- round up
                   | otherwise = []

    minimize :: [Plate] -> [Plate]
    minimize [] = []
    minimize [p] = [p]
    minimize ((P45  w1):(P45  w2):ps) = minimize $ P45  (w1+w2):ps
    minimize ((P25  w1):(P25  w2):ps) = minimize $ P25  (w1+w2):ps
    minimize ((P10  w1):(P10  w2):ps) = minimize $ P10  (w1+w2):ps
    minimize ((P5   w1):(P5   w2):ps) = minimize $ P5   (w1+w2):ps
    minimize ((P2p5 w1):(P2p5 w2):ps) = minimize $ P2p5 (w1+w2):ps
    minimize (p:ps) = p:minimize ps
