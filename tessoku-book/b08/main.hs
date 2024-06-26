module Main where

import Control.Monad ( replicateM )
import Data.Array ( Array, (!), listArray, accumArray, elems, bounds )
import Data.List.Split ( chunksOf )

twoDimensionalSum :: Int -> Int -> Array (Int, Int) Int -> Array (Int, Int) Int
twoDimensionalSum w h = listArray ((0, 0), (w, h))
                      . concat
                      . scanl (zipWith (+)) (replicate (succ h) 0)
                      . map (scanl (+) 0)
                      . chunksOf h
                      . elems

solve :: Array (Int, Int) Int -> (Int, Int, Int, Int) -> Int
solve twoDimSum (a, b, c, d) = twoDimSum ! (a - 1, b - 1) + twoDimSum ! (c, d) 
                             - twoDimSum ! (c, b - 1) - twoDimSum ! (a - 1, d)

main :: IO ()
main = do
    n <- readLn
    xys <- map ((\[x, y] -> ((x, y), 1)) . map read . words) <$> replicateM n getLine
    q <- readLn
    qs <- map ((\[a, b, c, d] -> (a, b, c, d)) . map read . words) <$> replicateM q getLine
    let w = foldl (\acc ((x, _), _) -> max acc x) 1 xys
    let h = foldl (\acc ((_, y), _) -> max acc y) 1 xys
    let twoDimSum = twoDimensionalSum w h $ accumArray (+) 0 ((1, 1), (w, h)) xys
    mapM_ (print . solve twoDimSum) qs
