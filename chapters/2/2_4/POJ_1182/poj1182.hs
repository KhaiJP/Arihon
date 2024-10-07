import Data.DisjointSet as DSU ( empty, equivalent, insert, union, DisjointSet )
import Control.Monad ( replicateM )


type UF = DSU.DisjointSet (Index, Char)
type Index = Int


main :: IO ()
main = do
    [n, k] :: [Int] <- map read . words <$> getLine
    queries :: [[Int]] <- replicateM k $ map read . words <$> getLine
    let update' = update'' n    
    let uf = initUF n
    let (ans, _) = foldl update' (0, uf) queries
    print ans


initUF :: Int -> UF
initUF n = foldl (flip DSU.insert) DSU.empty [ (i, c) | i <- [1..n], c <- ref1 ]


update'' :: Int -> (Int, UF) -> [Int] -> (Int, UF)
update'' n (c, uf) [t, x, y]
    | isInvalid = (c + 1, uf)
    | t == 1    = subprocess1 c uf x y
    | t == 2    = subprocess2 c uf x y
    | otherwise = (c, uf)
    where isInvalid = x < 1 || n < x || y < 1 || n < y


subprocess1 :: Int -> UF -> Index -> Index -> (Int, UF)
subprocess1 c uf x y
    | checkType1 uf x y = (c, processType1 uf x y)
    | otherwise = (c + 1, uf)


checkType1 :: UF -> Index -> Index -> Bool
checkType1 uf x y = and [ isOK c1 c2 | c1 <- ref1, c2 <- ref1, c1 /= c2 ]
    where isOK c1 c2 = not $ DSU.equivalent (x, c1) (y, c2) uf


processType1 :: UF -> Index -> Index -> UF
processType1 uf x y = foldl (\u c -> DSU.union (x, c) (y, c) u) uf ref1


subprocess2 :: Int -> UF -> Index -> Index -> (Int, UF)
subprocess2 c uf x y
    | checkType2 uf x y = (c, processType2 uf x y)
    | otherwise = (c + 1, uf)


checkType2 :: UF -> Index -> Index -> Bool
checkType2 uf x y = and [ isOK c1 c2 | (c1, c2) <- ref3 ]
    where isOK c1 c2 = not $ DSU.equivalent (x, c1) (y, c2) uf


processType2 :: UF -> Index -> Index -> UF
processType2 uf x y = foldl (\u (c1, c2) -> DSU.union (x, c1) (y, c2) u) uf ref2


ref1 :: String
ref1 = "ABC"


ref2 :: [(Char, Char)]
ref2 = [('A', 'B'), ('B', 'C'), ('C', 'A')]


ref3 :: [(Char, Char)]
ref3 = [('A', 'C'), ('A', 'A'), ('B', 'A'), ('B', 'A'), ('C', 'B'), ('C', 'C')]
