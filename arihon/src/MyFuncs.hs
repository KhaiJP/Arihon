{-
Module      : MyFuncs
Description : A collection of functions used in various problems
-}

module MyFuncs (
    generalizedBinarySearch,
    intBinarySearch',
    realBinarySearch',
    triangleProduct
) where


-- | Performs binary search on real numbers.
-- 
-- Takes an allowable error and a function to determine if the condition is met,
-- and returns a value that meets the condition.
realBinarySearch' :: (Ord float, Floating float)
                  => float             -- ^ Allowable error
                  -> (float -> Bool)   -- ^ Function to determine if the condition is met 
                  -> float             -- ^ Value that meets the condition
                  -> float             -- ^ Value that does not meet the condition
                  -> float             -- ^ Minimum/Maximum value that meets the condition
realBinarySearch' epsilon = generalizedBinarySearch getMid closeEnough
    where
        getMid ok ng      = (ok + ng) / 2
        closeEnough ok ng = abs (ok - ng) < epsilon

-- | Performs binary search on integers.
--
-- Takes a function to determine if the condition is met,
-- and returns a value that meets the condition.
intBinarySearch' :: Integral int
                 => (int -> Bool)  -- ^ Function to determine if the condition is met
                 -> int            -- ^ Value that meets the condition
                 -> int            -- ^ Value that does not meet the condition
                 -> int            -- ^ Minimum/Maximum value that meets the condition
intBinarySearch' = generalizedBinarySearch getMid closeEnough
    where
        getMid ok ng      = (ok + ng) `div` 2
        closeEnough ok ng = abs (ok - ng) == 1

-- | Performs generalized binary search.
--
-- Takes a function to get the middle value from two values,
-- and a function to determine if the binary search should end,
-- and returns a value that meets the condition.
generalizedBinarySearch :: (a -> a -> a)      -- ^ Function to get the middle value
                        -> (a -> a -> Bool)   -- ^ Function to determine if the binary search should end
                        -> (a -> Bool)        -- ^ Function to determine if the condition is met
                        -> a                  -- ^ Value that meets the condition
                        -> a                  -- ^ Value that does not meet the condition
                        -> a                  -- ^ Minimum/Maximum value that meets the condition
generalizedBinarySearch getMid closeEnough criterion ok ng
    | closeEnough ok ng = ok
    | criterion mid     = generalizedBinarySearch getMid closeEnough criterion mid ng
    | otherwise         = generalizedBinarySearch getMid closeEnough criterion ok mid
    where
        mid = getMid ok ng


-- | Calculates the product of the triangle number.
triangleProduct :: Integral int => int -> int
triangleProduct n = n * (n + 1) `div` 2
