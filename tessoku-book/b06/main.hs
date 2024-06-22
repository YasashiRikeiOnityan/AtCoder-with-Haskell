module Main where

import Control.Monad ( replicateM )
import Data.Array ( Array, (!), listArray )

cumulativeSum :: (Int, Int) -> [Int] -> Array Int Int
cumulativeSum lr = listArray lr . scanl (+) 0

luckUnluck :: Array Int Int -> (Int, Int) -> (Int, Int)
luckUnluck s (l, r) = (lucky, unlucky)
    where
        lucky = s ! r - s ! pred l
        unlucky = r - l + 1 - lucky

pair :: [Int] -> (Int, Int)
pair [x, y] = (x, y)
pair _ = undefined

out :: (Int, Int) -> String
out (lucky, unlucky)
    | lucky > unlucky  = "win"
    | lucky == unlucky = "draw"
    | otherwise        = "lose"

main :: IO ()
main = do
    n <- readLn
    as <- map read . words <$> getLine
    q <- readLn
    lrs <- map (pair . map read . words) <$> replicateM q getLine
    mapM_ (putStrLn . out . luckUnluck (cumulativeSum (0, n) as)) lrs