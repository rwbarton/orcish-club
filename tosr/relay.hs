{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Arrow
import Control.Monad.RWS
import Data.Char
import Data.List
import Data.Ord
import System.Environment
import System.IO

import Hand
import HandEval

data Strain = Suit Suit | NT deriving (Eq, Ord)
data Bid = Bid Int Strain deriving (Eq, Ord)

c, d, h, s, nt :: Strain
c = Suit C
d = Suit D
h = Suit H
s = Suit S
nt = NT

instance Bounded Bid where
  minBound = Bid 1c
  maxBound = Bid 7nt

instance Enum Bid where
  fromEnum (Bid n str) = 5 * n + fE str
    where fE (Suit s) = fromEnum s
          fE NT = 4
  toEnum x = Bid n (tE r)
    where (n, r) = divMod x 5
          tE y | y < 4 = Suit $ toEnum y
          tE 4 = NT

instance Show Bid where
  show (Bid n str) = show n ++ case str of
    Suit C -> "C"
    Suit D -> "D"
    Suit H -> "H"
    Suit S -> "S"
    NT -> "N"

data RelayState = RelayState { relayAsk :: Bid,
                               extraSteps :: Int }
newtype Relay a = Relay (RWS Hand [Bid] RelayState a)
                deriving (Functor, Monad, MonadReader Hand,
                          MonadWriter [Bid], MonadState RelayState)

runRelay :: Relay a -> Hand -> (a, RelayState, [Bid])
runRelay (Relay r) h = runRWS r h $ RelayState { relayAsk = Bid 1c, extraSteps = 0 }

skipSignoff :: Bid -> Bid
skipSignoff (Bid 3 NT) = Bid 4c
skipSignoff (Bid 4 (Suit D)) = Bid 4h
skipSignoff x = x

bid :: Int -> Strain -> Relay ()
bid l str = do
  extra <- extraSteps <$> get
  let b = (iterate succ $ Bid l str) !! extra
  tell [b]
  let b' = succ b
      b'' = skipSignoff b'
  put $ RelayState { relayAsk = b'', extraSteps = extra + if b'' /= b' then 1 else 0 }

zoom :: Int -> Strain -> Relay ()
zoom l str = do
  extra <- extraSteps <$> get
  let b = (iterate succ $ Bid l str) !! extra
  put $ RelayState { relayAsk = pred b, extraSteps = extra }

steps :: Int -> Relay ()
steps n = do
  r <- relayAsk <$> get
  let b = (iterate succ r) !! (n + 1)
      b' = skipSignoff $ succ b
  tell [b]
  modify $ \x -> x { relayAsk = b' }

zoomSteps :: Int -> Relay ()
zoomSteps n = do
  r <- relayAsk <$> get
  let b = (iterate succ r) !! (n + 1)
  modify $ \x -> x { relayAsk = pred b }



-- The system
tosr :: Relay ()
tosr = do
  hand <- ask
  if hcp hand >= 9 && ak hand >= 2
    then normalRelay normalRangeControls
    else do
    bid 1d
    if hcp hand >= 5
      then twoUpRelay
      else bid 1s

transfer :: Suit -> Relay ()
transfer S = bid 1h
transfer H = bid 1nt
transfer D = bid 2c
transfer C = bid 2d

normalRelay :: Relay () -> Relay ()
normalRelay rangeControls = do
  hand <- ask
  let sh = reverse $ shape hand
  if 2 <= minimum sh && maximum sh <= 4
    then do                    -- balanced
    bid 1s
    if minimum sh == 2
      then do                   -- 4432
      let longSuits = sort $ suitsOfLength hand 4
      case longSuits of
        [C, S] -> bid 2d
        [D, H] -> bid 2d
        [C, D] -> bid 2h
        [H, S] -> bid 2h
        _ -> return ()
      let [shortSuit] = suitsOfLength hand 2
      case shortSuit of
        S -> bid 2nt
        C -> bid 3c
        D -> bid 3d
        H -> zoom 3h
      else do                   -- 4333
      let [longSuit] = suitsOfLength hand 4
      when (longSuit == H || longSuit == S) $ bid 2d
      bid 2s
      if longSuit == C || longSuit == H then bid 3c else zoom 3d
    else case length (filter (>= 4) sh) of
    1 -> do                     -- one-suiter
      let [longSuit] = suitsOfAtLeast hand 5
      transfer longSuit
      let residue = map (suitLength hand) $ reverse suits \\ [longSuit]
          shortestLength = minimum residue
      when (residue == [2,2,3] || residue == [2,3,2]) $ bid 2s
      case map (== shortestLength) residue of
        [True, False, False] -> bid 2s
        [False, True, False] -> bid 2nt
        [False, False, True] -> return ()
        _ -> bid 3c
      case reverse $ sort residue of
        [3,3,2] -> bid 3d
        [3,3,1] -> bid 3h
        [3,3,0] -> bid 3s
        [3,2,1] -> zoom 3nt
        [2,2,2] -> bid 3h
        [3,2,2] -> if residue == [2,2,3] then bid 3h else zoom 3s
    2 -> do                     -- two-suiter
      let [hi, lo] = reverse . sort $ suitsOfAtLeast hand 4
          reverser = suitLength hand hi == 4
      case (hi, lo) of
        (H, D) -> bid 1s >> bid 2c
        (D, C) -> return ()
        (S, D) -> bid 1h >> bid 2c
        (S, H) -> bid 1h >> bid 1nt
        (a, C) -> transfer a >> when (not reverser) (bid 2d)
      when reverser $ bid 2h
      let residue = map (suitLength hand) $ reverse suits \\ [hi, lo]
          highShortness = residue !! 0 < residue !! 1
      if sh !! 1 == 4
        then do
        when highShortness $ bid 2nt
        case sh of
          [5,4,2,2] -> bid 3c
          [5,4,3,1] -> bid 3d
          [6,4,2,1] -> bid 3h
          [6,4,3,0] -> bid 3s
          [7,4,2,0] -> bid 3nt  -- XXX zoom with 7402?
          [7,4,1,1] -> zoom 4c
        else do
        bid 2s
        when highShortness $ bid 3c
        case sh of
          [5,5,2,1] -> bid 3h
          [5,5,3,0] -> bid 3s
          [6,5,2,0] -> if suitLength hand hi == 5 then bid 3nt else zoom 4c
          [6,5,1,1] -> bid 3d >> if suitLength hand hi == 5 then bid 3s else zoom 3nt
    3 -> do                     -- three-suiter
      let [shortSuit] = suitsOfLength hand (minimum sh)
      case shortSuit of
        C -> bid 1h >> bid 1nt >> bid 2d >> zoom 2nt
        D -> bid 1h >> bid 1nt >> bid 2d >> bid 2s
        H -> bid 2c >> bid 2h >> zoom 3c
        S -> bid 2c >> bid 2h >> bid 2nt
      let nonResidue = map (suitLength hand) $ reverse suits \\ [shortSuit]
      case nonResidue of
        [4,4,4] -> steps 0
        [4,4,5] -> steps 1
        [4,5,4] -> steps 2
        [5,4,4] -> zoomSteps 3
  -- range & controls
  rangeControls
  -- denial cue bidding
  let go [] = return ()
      go xs = do
        b <- relayAsk <$> get
        when (b <= Bid 5 NT) $ do
          let (f, False:xs') = span id xs
          steps (length f)
          go xs'
  go $ spiralScan hand ++ [False]

normalRangeControls :: Relay ()
normalRangeControls = do
  hand <- ask
  let strong = hcp hand >= 13 && ak hand >= 3 || ak hand >= 6 -- 3 Aces counts as strong
  if not strong
    then steps 0 >> (if ak hand < 5 then steps (ak hand - 2) else zoomSteps 3)
    else zoomSteps 1 >> steps (ak hand - 3)

spiralScan :: Hand -> [Bool]
spiralScan hand@(Hand h) = concat . transpose . map oneSuit $ suitOrder
  where suitOrder = reverse $ sortBy (comparing $ suitLength hand &&& id) suits
        oneSuit a = take visits $ if length ranks <= 5
                                  then ((A `elem` ranks) /= (K `elem` ranks)) : [ r `elem` ranks | r <- [Q, J, T] ]
                                  else (numAKQ >= 2) : odd numAKQ : [ r `elem` ranks | r <- [J, T] ]
          where ranks = [ r | Card b r <- h, b == a ]
                visits = min 3 $ length ranks - 1
                numAKQ = length $ intersect [A, K, Q] ranks

twoUpRelay :: Relay ()
twoUpRelay = do
  modify $ \x -> x { extraSteps = 2 }
  normalRelay twoUpRangeControls

twoUpRangeControls :: Relay ()
twoUpRangeControls = do
  hand <- ask
  steps $ ak hand

compareLength [] [] = EQ
compareLength [] _ = LT
compareLength _ [] = GT
compareLength (_:xs) (_:ys) = compareLength xs ys

main :: IO ()
main = do
  args <- getArgs
  case args of
    [hStr] -> do
      let hand = readHand . map toUpper $ hStr
          (_, _, bids) = runRelay tosr hand
      mapM_ print bids
    [] -> do
      hSetBuffering stdout NoBuffering
      hand <- randomHand
      putStrLn $ "You hold: " ++ show hand
      putStr "1C -- "
      let (_, _, bids) = runRelay tosr hand
      guesses <- takeWhile (/= "") . lines . map toUpper <$> getContents
      sequence_ [ do
                     when (a /= b) $ putStrLn $ "Wrong, should be " ++ b
                     putStr $ (show $ skipSignoff $ succ b0) ++ " -- "
                | (a, b0) <- zip guesses bids, let b = show b0 ]
      case compareLength guesses bids of
        GT -> putStrLn "Don't know where you're going with that last bid..."
        EQ -> putStrLn "You win!"
        LT -> putStrLn "You passed a relay ask..."
