module Main where

import Control.Monad ( replicateM )
import Data.Array ( Array, (!), listArray )

twoDimensionalSum :: Int -> Int -> [[Int]] -> Array (Int, Int) Int
twoDimensionalSum h w = listArray ((0, 0), (h, w)) 
                      . concat 
                      . scanl (zipWith (+)) (replicate (w + 1) 0) 
                      . map (scanl (+) 0)

solve :: Array (Int, Int) Int -> [(Int, Int, Int, Int)] -> [Int]
solve twoDimSum = map (\(x, y, z, w) -> 
    twoDimSum ! (x - 1, y - 1) + twoDimSum ! (z, w)
    - twoDimSum ! (z, y - 1) - twoDimSum ! (x - 1, w))

quadruple :: [Int] -> (Int, Int, Int, Int)
quadruple [x, y, z, w] = (x, y, z, w)
quadruple _ = undefined

main :: IO ()
main = do
    [h, w] <- map read . words <$> getLine
    xs <- map (map read . words) <$> replicateM h getLine
    q <- readLn
    qs <- map (quadruple . map read . words) <$> replicateM q getLine
    mapM_ print . solve (twoDimensionalSum h w xs) $ qs
