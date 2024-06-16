module Main where

buildings :: Int -> [Int] -> Int -> Int
buildings _ [] _ = -1
buildings n (h : hs) i
    | n < h = i
    | otherwise = buildings n hs (succ i)

main :: IO ()
main = do
    _ <- getLine
    (h : hs) <- map read . words <$> getLine
    print $ buildings h hs 2
