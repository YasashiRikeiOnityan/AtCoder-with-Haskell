module Main where

import Data.List ( sort )

(>+<) :: [Int] -> [Int] -> [Int]
(>+<) [] _ = []
(>+<) _ [] = []
(>+<) (x : xs) yys@(y : ys)
    | x < y = (>+<) xs yys
    | otherwise = x : (>+<) xs ys

main :: IO ()
main = do
    [_, m] <- map read . words <$> getLine
    as <- map read . words <$> getLine
    bs <- map read . words <$> getLine
    let merged = (>+<) (sort as) (sort bs)
    print $ if null merged || length merged /= m then -1 else sum merged
