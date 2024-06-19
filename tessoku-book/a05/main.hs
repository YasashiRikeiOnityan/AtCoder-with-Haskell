module Main where

a05 :: Int -> Int -> Int
a05 n k = length [(x, y, z) | x <- [1..n], y <- [1..n], let z = k - x - y, 1 <= z, z <= n]

main :: IO ()
main = do
    [n, k] <- map read . words <$> getLine
    print $ a05 n k
