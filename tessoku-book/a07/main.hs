module Main where

import Control.Monad ( replicateM )
import Data.Array ( accumArray, elems, Array )

add2PreviousDay :: [Int] -> [(Int, Int)]
add2PreviousDay [l, r] = [(l, 1), (succ r, -1)]
add2PreviousDay _ = []

cumulativeSum :: Array Int Int -> [Int]
cumulativeSum = scanl (+) 0 . elems

main :: IO ()
main = do
    d <- readLn
    n <- readLn
    lrs <- concatMap (add2PreviousDay . fmap read . words) <$> replicateM n getLine
    mapM_ print . tail . init . cumulativeSum . accumArray (+) 0 (1, succ d) $ lrs