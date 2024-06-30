module Main where

import Data.Array ( listArray, (!) )

dungeon :: Int -> [Int] -> [Int] -> Int
dungeon n xs ys = dp ! n
    where
        xrr = listArray (1, pred n) xs
        yrr = listArray (1, (pred . pred) n) ys

        dp = listArray (1, n) [cost i | i <- [1..n]]

        cost 1 = 0
        cost 2 = xrr ! 1
        cost i = min (dp ! s + xrr ! s) (dp ! t + yrr ! t)
            where
                s = pred i
                t = (pred . pred) i

main :: IO ()
main = do
    n <- readLn
    as <- map read . words <$> getLine
    bs <- map read . words <$> getLine
    print $ dungeon n as bs
