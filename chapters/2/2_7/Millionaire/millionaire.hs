import qualified Data.Map.Strict as M

type DP    = M.Map (NTry, Class) Double
type NTry  = Int
type Class = Int


main :: IO ()
main = do
    [sm, sp, sx] <- words <$> getLine
    let m :: Int = read sm
    let p :: Double = read sp
    let x :: Double = read sx
    print $ solve p goal m x


solve :: Double -> Double -> NTry -> Double -> Double
solve p g m x = fdp M.! (0, 1)
    where
        fdp = foldl (update' p) (initDpTable g m x) (allPatterns m)


update' :: Double -> DP -> (NTry, Class) -> DP
update' p dp (n, c)
    | n == 0           = dp
    | c == 2^n         = M.update (\y -> Just (y + currValue)) (n-1, 2^(n-1)) dp
    | otherwise = dp''
    where
        dp'  = M.update (\y -> Just (y + p * currValue)) (n-1, c') dp
        dp'' = M.update (\y -> Just (y + (1-p) * currValue)) (n-1, c'') dp'
        c'   = min (2^(n-1)) $ (c `div` 2) + 1
        c''  = max 0 $ (c - 1) `div` 2
        currValue = dp M.! (n, c)


allPatterns :: NTry -> [(NTry, Class)]
allPatterns m = [(n, c) | n <- reverse [1..m], c <- [1..2^n]]


initDpTable :: Double -> NTry -> Double -> DP
initDpTable g m x = M.update (\_ -> Just 1) (m, initClass) allZero
    where
        allZero = M.fromList [((n, c), 0) | n <- [0..m], c <- [0..2^n]]
        initClass = getClass g m x


getClass :: Double -> Int -> Double -> Int
getClass g m x = fst . head . filter (\(_, y) -> y <= x) $ zip [2^m, 2^m-1..] l
    where l :: [Double] = getBounds g m


getBounds :: Double -> Int -> [Double]
getBounds g m = reverse $ map f [0..2^m]
    where f = (*g) . (/2^m)

goal :: Double
goal = 10^6
