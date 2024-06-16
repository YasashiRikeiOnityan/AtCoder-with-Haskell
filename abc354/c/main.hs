module Main where

import Control.Monad ( replicateM )
import Data.List ( sortBy )
import Data.Ord ( comparing )

merge :: [(Int, (Int, Int))] -> [(Int, (Int, Int))] -> [Int]
merge [] [] = []
merge [] ((i, ac) : acs) = i: merge [] acs
merge ((i, ac) : acs) [] = i : merge acs []
merge ((i, (a1, c1)) : ac1) acs2@((_, (a2, c2)) : ac2) = case compare a1 a2 of
    GT -> case compare c1 c2 of
        LT -> i : merge ac1 ac2
        _  -> merge ac1 acs2
    _  -> merge ac1 acs2

main :: IO ()
main = do
    n <- readLn
    xys <- map (pair . map read . words) <$> replicateM n getLine
    let ans = merge (zip [1..] (sortBy (comparing fst) xys)) (zip [1..] (sortBy (comparing fst) xys))
    print $ length ans
    putStrLn . unwords . map show $ ans

pair :: [a] -> (a, a)
pair [x, y] = (x, y)
pair _ = (undefined, undefined)