module Main where

import Control.Monad ( replicateM )
import Data.Array ( Array, (!), listArray )

cumulativeSum :: (Int, Int) -> [Int] -> Array Int Int
cumulativeSum lr = listArray lr . scanl (+) 0

numOfVisitors :: Array Int Int -> (Int, Int) -> Int
numOfVisitors s (l, r) = s ! r - s ! pred l

pair :: [Int] -> (Int, Int)
pair [x, y] = (x, y)
pair _ = undefined

main :: IO ()
main = do
    [n, q] <- map read . words <$> getLine
    as <- map read . words <$> getLine
    lrs <- map (pair . map read . words) <$> replicateM q getLine
    mapM_ (print . numOfVisitors (cumulativeSum (0, n) as)) lrs