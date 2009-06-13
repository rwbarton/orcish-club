import Control.Monad.Instances
import Data.List
import Data.Ratio
import Text.Printf

deals :: Integer
deals = product [1..52] `div` (product [1..13])^4

choose :: Integer -> Integer -> Integer
n `choose` k = product [n-k+1..n] `div` product [1..k]

p :: [Integer] -> Rational
p [s,h,d,c] = choose 13 s * choose 13 h * choose 13 d * choose 13 c * choose 39 13 * choose 26 13 % deals

p2 :: [Integer] -> [Integer] -> Rational
p2 x1 x2 = product [ choose 13 i1 * choose (13-i1) i2 | (i1, i2) <- zip x1 x2 ] * choose 26 13 % deals

orderedPartitions :: Integer -> Integer -> [[Integer]]
orderedPartitions 0 0 = [[]]
orderedPartitions _ 0 = []
orderedPartitions n k = [ x : p | x <- [0..n], p <- orderedPartitions (n-x) (k-1) ]

shapes = orderedPartitions 13 4

possible [s,h,d,c] = s <= 10 && h <= 10 && c <= 9

-- probability that partner has x1 given that you have x2
cond x1 x2 = p2 x1 x2 / p x2

shapes2d = [[4,4,1,4], [4,3,1,5], [3,4,1,5], [4,4,0,5]]

-- probability that partner has x1 given that you opened 2d
cond2d x1 = sum [ p2 x1 x2 | x2 <- shapes2d ] / sum [ p x2 | x2 <- shapes2d ]

dist :: [Integer] -> [([Integer], Rational)]
dist x1 = map (fmap (/ sum (map snd probs))) probs
  where probs = [ (opener, cond opener x1) | opener <- shapes2d ]

-- for every possible shape
-- work out the best strategy opposite passing responder & opposite correcting responder
-- and probability of success in each case

level :: Int -> Integer         -- where can we play -- at least 2D
level 0 = 8
level 1 = 8
level 2 = 8
level 3 = 9

-- Average levels in excess of LOTT level (0 is perfect)
lottPass :: [Integer] -> Rational
lottPass x1@[s,h,d,c] = minimum [ sum [ p * (fromInteger $ max 0 (level i - (opener !! i + x1 !! i))) | (opener, p) <- dist x1 ] | i <- [0, 1, 2, 3] ]

lottCorrect :: [Integer] -> Rational
lottCorrect x1@[s,h,d,c] = minimum [ sum [ p * (fromInteger $ max 0 (
                                                   if i == 1 && opener !! i == 3
                                                   then -- bid hearts; opener corrects with 3 and we get to the best spot but must play at least 2S
                                                     minimum [ (if j == 0 then 8 else 9) - (opener !! j + x1 !! j) | j <- [0, 1, 2, 3] ]
                                                   else -- bid something else, opener passes
                                                     level i - (opener !! i + x1 !! i)
                                               ))
                                         | (opener, p) <- dist x1
                                         ] | i <- [0, 1, 2, 3] ]

main = mapM_ (\(diff, lc, lp, s) -> printf "%7.4f %s %8.3f%%\n" (frac diff) (show s) (frac $ 100 * cond2d s)) $
       reverse $ sort [ (lc - lp, lc, lp, s) | s <- shapes, possible s, let lc = lottCorrect s, let lp = lottPass s, lc /= lp ]
         where frac x = realToFrac x :: Double
