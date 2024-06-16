module Main where

import Control.Monad ( replicateM )

main :: IO ()
main = do
    n <- readLn
    [x, y] <- map read . words <$> getLine :: IO [Int]
    xys <- map (pair . map read . words) <$> replicateM n getLine :: IO [(Int, Int)]
    print 0

pair :: [a] -> (a, a)
pair [x, y] = (x, y)
pair _ = (undefined, undefined)