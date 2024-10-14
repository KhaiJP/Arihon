{- |
Module      : Main
Description : 2008 ACPC local onsite C, Millionaire
-}
module Main where

import qualified Data.Map.Strict as M

type DP     = M.Map (NumTry, Level) Prob
type NumTry = Int
type Level  = Int
type Prob   = Double
type Money  = Double


-- | Takes the total number of rounds, the winning probability for each round (constant),
-- and the initial amount of money, and outputs the probability of winning in the end.
main :: IO ()
main = do
    [sm, sp, sx]    <- words <$> getLine
    let m :: Int   = read sm
    let p :: Prob  = read sp
    let x :: Money = read sx
    print $ solve p (goal, x) m


-- | Calculates the probability of winning the game given the probability of winning each round,
-- the goal amount, and the initial amount of money.
solve :: Prob            -- ^ The probability of winning each round
      -> (Money, Money)  -- ^ (Goal amount, Initial amount of money)
      -> NumTry          -- ^ The total number of rounds
      -> Prob            -- ^ The probability of winning the whole game
solve p (g, x) m = resultDP M.! (0, 1)
    where
        resultDP = foldl (makeBet p) (initDPTable (g, x) m) schedule
        schedule = getSchedule m


-- | Makes a bet based on the current value and updates the dp table
makeBet :: Prob             -- ^ The probability of winning each round
        -> DP               -- ^ The dp table
        -> (NumTry, Level)  -- ^ (Remaining number of rounds, Current level)
        -> DP               -- ^ The updated dp table
makeBet p dp (n, lv)
    | n == 0    = dp
    | lv == 2^n = M.update (Just . (+eVal)) (n-1, 2^(n-1)) dp
    | otherwise = ndp
    where
        ndp  = foldl (getBetResults n eVal) dp next
        next = [(p, (lv `div` 2) + 1), (1-p, (lv - 1) `div` 2)]
        eVal = dp M.! (n, lv)


-- | Updates the dp table with the result of a bet
getBetResults :: NumTry          -- ^ Remaining number of rounds
              -> Prob            -- ^ Current expected value
              -> DP              -- ^ dp table
              -> (Prob, Level)   -- ^ (Probability, Current level)
              -> DP              -- ^ The updated dp table
getBetResults n eVal dp (p, lv) = M.update (Just . (+p*eVal)) (n-1, lv) dp


-- | Calculates all combinations of number of rounds and levels from the total number of rounds
getSchedule :: NumTry             -- ^ The total number of rounds
            -> [(NumTry, Level)]  -- ^ The list of all combinations of number of rounds and levels
getSchedule m = [(n, lv) | n <- reverse [1..m], lv <- [1..2^n]]


-- | Initializes the dp table
initDPTable :: (Money, Money)  -- ^ (Goal amount, Initial amount of money)
            -> NumTry          -- ^ The total number of rounds
            -> DP              -- ^ The initialized dp table
initDPTable (g, x) m = M.update (\_ -> Just 1) (m, initLevel) allZero
    where
        allZero   = M.fromList [((n, lv), 0) | n <- [0..m], lv <- [0..2^n]]
        initLevel = getLevel (g, x) m


-- | Calculates the level of the money
getLevel :: (Money, Money)  -- ^ (Goal amount, Initial amount of money)
         -> NumTry          -- ^ The total number of rounds
         -> Level           -- ^ The level of the money
getLevel (g, x) m = fst . head . filter (\(_, y) -> y <= x) $ zip [2^m, 2^m-1..] l
    where
        l = reverse $ map f [0..2^m]
        f = (*g) . (/2^m)


-- | The goal amount of money
goal :: Money
goal = 10^6
