module Main where

import Control.Monad ( replicateM )
import Data.List ( sortBy )
import Data.Ord ( comparing )

solve :: Int -> [(String, Int)] -> String
solve n scs = fst $ scs !! (sum' scs `mod` n)

sum' :: [(String, Int)] -> Int
sum' = foldr ((+) . snd) 0

main :: IO ()
main = do
    n <- readLn
    scs <- map (pair . words) <$> replicateM n getLine
    putStrLn $ solve n (sortBy (comparing fst) scs)

pair :: [String] -> (String, Int)
pair [x, y] = (x, read y)
pair _ = (undefined, undefined)