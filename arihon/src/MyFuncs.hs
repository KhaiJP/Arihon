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
--
-- @param epsilon Allowable error
-- @param criterion Function to determine if the condition is met
-- @param ok Value that meets the condition
-- @param ng Value that does not meet the condition
-- @return Value that meets the condition
realBinarySearch' :: (Ord float, Floating float) => float -> (float -> Bool) -> float -> float -> float
realBinarySearch' epsilon = generalizedBinarySearch getMid closeEnough
    where
        getMid ok ng      = (ok + ng) / 2
        closeEnough ok ng = abs (ok - ng) < epsilon

-- | Performs binary search on integers.
--
-- Takes a function to determine if the condition is met,
-- and returns a value that meets the condition.
--
-- @param criterion Function to determine if the condition is met
-- @param ok Value that meets the condition
-- @param ng Value that does not meet the condition
-- @return Value that meets the condition
intBinarySearch' :: Integral int => (int -> Bool) -> int -> int -> int
intBinarySearch' = generalizedBinarySearch getMid closeEnough
    where
        getMid ok ng      = (ok + ng) `div` 2
        closeEnough ok ng = abs (ok - ng) == 1

-- | Performs generalized binary search.
--
-- Takes a function to get the middle value from two values,
-- and a function to determine if the binary search should end,
-- and returns a value that meets the condition.
--
-- @param getMid Function to get the middle value
-- @param closeEnough Function to determine if the binary search should end
-- @param criterion Function to determine if the condition is met
-- @param ok Value that meets the condition
-- @param ng Value that does not meet the condition
-- @return Value that meets the condition
generalizedBinarySearch :: (a -> a -> a) -> (a -> a -> Bool) -> (a -> Bool) -> a -> a -> a
generalizedBinarySearch getMid closeEnough criterion ok ng
    | closeEnough ok ng = ok
    | criterion mid     = generalizedBinarySearch getMid closeEnough criterion mid ng
    | otherwise         = generalizedBinarySearch getMid closeEnough criterion ok mid
    where
        mid = getMid ok ng


triangleProduct :: Integral int => int -> int
triangleProduct n = n * (n + 1) `div` 2
