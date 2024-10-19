{- |
Module: Subsequence
Description: Solve POJ 3061, Subsequence
-}

module Subsequence where

import qualified Data.Sequence as Sq

type Deq = Sq.Seq Int


main :: IO ()
main = do
    _           <- getLine
    s  :: Int   <- readLn
    xs :: [Int] <- map read . words <$> getLine
    let ans = solve s xs
    print $ if ans == maxBound then 0 else ans


solve :: Int
      -> [Int]
      -> Int
solve s = twoPointers' s Sq.empty 0 (maxBound :: Int)


twoPointers' :: Int
             -> Deq
             -> Int
             -> Int
             -> [Int]
             -> Int
twoPointers' s q n ans l
    | null l && n < s = ans
    | s <= n          = twoPointers' s q' (n-h) ans' l
    | otherwise       = twoPointers' s (q Sq.|> x) (n+x) ans xs
    where
        Just (h, q') = headQ q
        ans'         = min ans $ Sq.length q
        (x:xs)       = l


headQ :: Deq -> Maybe (Int, Deq)
headQ q = case Sq.viewl q of
    Sq.EmptyL  -> Nothing
    x Sq.:< q' -> Just (x, q')
