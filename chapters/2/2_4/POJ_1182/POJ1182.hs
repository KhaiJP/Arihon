{- |
Module      : POJ1182
Description : POJ 1182, Food Chain
-}

module POJ1182 where

import qualified Control.Monad as CM ( replicateM )
import qualified Data.DisjointSet as DSU ( empty, equivalent, insert, union, DisjointSet )
import qualified Data.Bifunctor as BF ( first, second )

type UF     = DSU.DisjointSet (Index, Char)
type State  = (Int, UF)
type Index  = Int
type IdPair = (Index, Index)


-- | The main function
--
-- The input must have form;
--
-- N K
--
-- type1 x1 y1
--
-- type2 x2 y2
--
-- ...
--
-- typeK xK yK
main :: IO ()
main = do
    [n, k] :: [Int] <- map read . words <$> getLine
    queries :: [[Int]] <- CM.replicateM k $ map read . words <$> getLine
    let dq = dealQuery n
    let uf = initUF n
    let (ans, _) = foldl dq (0, uf) queries
    print ans


-- | Initializes the disjoint set
initUF :: Int  -- ^ The number of individuals
       -> UF   -- ^ The initial disjoint set
initUF n = foldl (flip DSU.insert) DSU.empty [ (i, c) | i <- [1..n], c <- "RSP" ]


-- | deals with the query and updates the state
dealQuery :: Int    -- ^ The number of individuals
          -> State  -- ^ The current number of invalid queries and the disjoint set
          -> [Int]  -- ^ The query
          -> State  -- ^ The updated number of invalid queries and the disjoint set
dealQuery n current_state [t, x, y]
    | isInvalid = new_state
    | t == 1    = process ref1 ref1_anti current_state (x, y)
    | t == 2    = process ref2 ref2_anti current_state (x, y)
    | otherwise = current_state
    where
        isInvalid = x < 1 || n < x || y < 1 || n < y
        new_state = BF.first (+ 1) current_state


-- | Processes the first type of query
process :: [(Char, Char)]  -- ^ Char pairs that define which chars to be connected
        -> [(Char, Char)]  -- ^ Char pairs that define which chars are not to be connected
        -> State           -- ^ The current state
        -> IdPair          -- ^ The indecies that are queried
        -> State           -- ^ The updated state
process ref_c ref_a cs ids
    | consistent = BF.second (update ref_c ids) cs
    | otherwise  = BF.first (+ 1) cs
    where consistent = check ref_a ids (snd cs)


-- | Updates the disjoint set
update :: [(Char, Char)]  -- ^ Char pairs that define which chars to be connected
       -> IdPair          -- ^ The indecies that are queried
       -> UF              -- ^ The current disjoint set
       -> UF              -- ^ The updated disjoint set
update ref (x, y) uf = foldl (\u (c1, c2) -> DSU.union (x, c1) (y, c2) u) uf ref


-- | Checks if the new query is consistent to the history
check ::  [(Char, Char)]  -- ^ Char pairs that define which chars are not to be connected
      -> IdPair           -- ^ The indecies that are queried
      -> UF               -- ^ The disjoint set
      -> Bool             -- ^ If the query is consistent to the history
check ref (x, y) uf= and [ isOK c1 c2 | (c1, c2) <- ref]
    where isOK c1 c2 = not $ DSU.equivalent (x, c1) (y, c2) uf


-- | Chars that are to be connected for type1 queries
ref1 :: [(Char, Char)]
ref1 = [('R', 'R'), ('S', 'S'), ('P', 'P')]


-- | Chars that are not to be connected for type1 queries
ref1_anti :: [(Char, Char)]
ref1_anti = [('R', 'S'), ('R', 'P'), ('S', 'P'), ('S', 'R'), ('P', 'R'), ('P', 'S')]


-- | Chars that are to be connected for type2 queries
ref2 :: [(Char, Char)]
ref2 = [('R', 'S'), ('S', 'P'), ('P', 'R')]


-- | Chars that are not to be connected for type2 queries
ref2_anti :: [(Char, Char)]
ref2_anti = [('R', 'P'), ('R', 'R'), ('S', 'R'), ('S', 'S'), ('P', 'S'), ('P', 'P')]
