module HandEval (
  hcp,
  ak,
  shape,
  suitLength,
  suitsOfLength,
  suitsOfAtLeast
  ) where

import Data.List
import Data.Maybe

import Hand

generalPoints :: Num a => [(Rank, a)] -> Hand -> a
generalPoints ps (Hand h) = sum $ do
  Card _ r <- h
  return $ fromMaybe 0 $ lookup r ps

hcp :: Hand -> Int
hcp = generalPoints [(A, 4), (K, 3), (Q, 2), (J, 1)]

ak :: Hand -> Int
ak = generalPoints [(A, 2), (K, 1)]

shape :: Hand -> [Int]
shape hand = sort [ suitLength hand a | a <- suits ]

suitLength :: Hand -> Suit -> Int
suitLength (Hand h) suit = length [ x | Card a x <- h, a == suit ]

suitsOfLength :: Hand -> Int -> [Suit]
suitsOfLength hand n = filter ((== n) . suitLength hand) suits

suitsOfAtLeast :: Hand -> Int -> [Suit]
suitsOfAtLeast hand n = filter ((>= n) . suitLength hand) suits
