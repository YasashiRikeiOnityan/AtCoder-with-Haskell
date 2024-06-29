module Main where

import Control.Monad ( replicateM )
import Data.Array ( Array, listArray, (!), inRange )

blockGame :: Int -> Array Int (Int, Int) -> Int
blockGame n parr = maximum [table ! l ! (n - l) | l <- [0..n]]
    where
        table = listArray (0, n) [listArray (0, n - l) [score l r | r <- [0..n-l]] | l <- [0..n]]

        score 0 0 = 0
        score l r
            | l == 0 = score2
            | r == 0 = score1
            | otherwise = max score1 score2
            where
                score1 = table ! pred l ! r + if inRange (l, n - r) (fst (parr ! l)) then snd (parr ! l) else 0
                score2 = table ! l ! pred r + if inRange (succ l, n - pred r) (fst (parr ! (n - pred r))) then snd (parr ! (n - pred r)) else 0


pair :: [a] -> (a, a)
pair [x, y] = (x, y)
pair _ = (undefined, undefined)

main :: IO ()
main = do
    n <- readLn
    pas <- map (pair . map read . words) <$> replicateM n getLine
    print . blockGame n . listArray (1, n) $ pas