module PlateCalc(displayPlateCalc) where

import Data.List (intersperse)

data Plates = Plates [Plate] deriving Show

data Plate = P45 Int | P25 Int | P10 Int | P5 Int | P2p5 Int | TooLight deriving Show


displayPlateCalc = displayPlates . plateCalc

displayPlates :: Plates -> String
displayPlates (Plates []) = "just the bar"
displayPlates (Plates ps) = dPlates ps
  where
    dPlates :: [Plate] -> String
    dPlates = concat . intersperse "," . map dPlate

    dPlate :: Plate -> String
    dPlate (TooLight) = "less than a bar"
    dPlate (P45  n) = "45x"  ++ show n
    dPlate (P25  n) = "25x"  ++ show n
    dPlate (P10  n) = "10x"  ++ show n
    dPlate (P5   n) = "5x"   ++ show n
    dPlate (P2p5 n) = "2.5x" ++ show n



plateCalc :: Int -> Plates
plateCalc lbs | lbs < 45 = Plates [TooLight]
              | lbs `mod` 5 /= 0 = plateCalc $ roundToNearest 5 lbs
              | otherwise = Plates $ minimize $ plateCalc' (lbs - 45)

  where
    plateCalc' :: Int -> [Plate]
    plateCalc' lbs | lbs >= 90 = P45  2:plateCalc' (lbs - 90)
                   | lbs >= 50 = P25  2:plateCalc' (lbs - 50)
                   | lbs >= 20 = P10  2:plateCalc' (lbs - 20)
                   | lbs >= 10 = P5   2:plateCalc' (lbs - 10)
                   | lbs >= 5  = P2p5 2:plateCalc' (lbs - 5)
                   | otherwise = []

    minimize :: [Plate] -> [Plate]
    minimize [] = []
    minimize [p] = [p]
    minimize ((P45  w1):(P45  w2):ps) = P45  (w1+w2):minimize ps
    minimize ((P25  w1):(P25  w2):ps) = P25  (w1+w2):minimize ps
    minimize ((P10  w1):(P10  w2):ps) = P10  (w1+w2):minimize ps
    minimize ((P5   w1):(P5   w2):ps) = P5   (w1+w2):minimize ps
    minimize ((P2p5 w1):(P2p5 w2):ps) = P2p5 (w1+w2):minimize ps
    minimize (p:ps) = p:minimize ps

    roundToNearest :: Int -> Int -> Int
    roundToNearest num i | num <= 0 = error "invalid"
                         | otherwise = 
      let rem = i `mod` num 
      in if rem < 3 then i - rem else i + (num - rem)
