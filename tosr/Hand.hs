module Hand (
  Suit(..),
  Rank(..),
  Card(..),
  Hand(..),
  suits,
  ranks,
  deck,
  readHand,
  randomHand
  ) where

import Control.Applicative
import Data.List
import System.Random

data Suit = C | D | H | S deriving (Eq, Ord, Show, Enum)
data Rank = Spot Integer | T | J | Q | K | A deriving (Eq, Ord)
data Card = Card Suit Rank deriving Eq
newtype Hand = Hand [Card]

suits = [C, D, H, S]
ranks = map Spot [2..9] ++ [T,J,Q,K,A]
deck = Card <$> suits <*> ranks

instance Show Rank where
  show (Spot n) = show n
  show T = "T"
  show J = "J"
  show Q = "Q"
  show K = "K"
  show A = "A"

instance Show Card where
  show (Card s r) = show s ++ show r

instance Show Hand where
  show (Hand cs) = intercalate "." [ concat [ show r | r <- reverse ranks, Card s r `elem` cs ]
                                   | s <- reverse suits ]

readHand :: String -> Hand
readHand str = Hand $ concat $ zipWith (map . Card) (reverse suits) (map readSuit pieces)
  where readSuit = map readCard
        readCard 'T' = T
        readCard 'J' = J
        readCard 'Q' = Q
        readCard 'K' = K
        readCard 'A' = A
        readCard n = Spot $ read [n]
        pieces = lines $ map (\x -> if x == '.' then '\n' else x) str

randomHand :: IO Hand
randomHand = Hand <$> choose 13 deck
  where choose 0 _ = return []
        choose n x@(x0:xs) = do
          a <- getStdRandom $ randomR (1, length x)
          if a <= n
            then (x0 :) <$> choose (n-1) xs
            else choose n xs
