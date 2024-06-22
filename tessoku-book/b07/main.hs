module Main where

import Control.Monad ( replicateM )
import Data.Array ( accumArray, elems, Array )

attendAndLeave :: [Int] -> [(Int, Int)]
attendAndLeave [l, r] = [(l, 1), (r, -1)]
attendAndLeave _ = []

cumulativeSum :: Array Int Int -> [Int]
cumulativeSum = scanl (+) 0 . elems

main :: IO ()
main = do
    t <- readLn
    n <- readLn
    lrs <- concatMap (attendAndLeave . map read . words) <$> replicateM n getLine
    mapM_ print . tail . init . cumulativeSum . accumArray (+) 0 (0, t) $ lrs