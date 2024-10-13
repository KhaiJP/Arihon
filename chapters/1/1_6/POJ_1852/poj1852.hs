main :: IO ()
main = do
    l :: Int <- readLn
    n :: Int <- readLn
    positions :: [Int] <- map read . words <$> getLine 
    let minTime = maximum . map (chooseCloser' l)
    let maxTime = maximum . map (chooseFarther' l)
    print (minTime positions, maxTime positions)


chooseCloser' :: Int -> Int -> Int
chooseCloser' l pos = min pos (l - pos)


chooseFarther' :: Int -> Int -> Int
chooseFarther' l pos = max pos (l - pos)
