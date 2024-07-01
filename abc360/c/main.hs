module Main where

import Data.List ( sort )

moveIt :: [(Int, Int)] -> Int
moveIt = sum . cost []

cost :: [(Int, Int)] -> [(Int, Int)] -> [Int]
cost tmp [] = [sum (map snd tmp) - maximum (map snd tmp)]
cost tmp (x : xs)
    | null tmp = cost [x] xs 
    | fst (head tmp) == fst x = cost (x : tmp) xs
    | otherwise = sum (map snd tmp) - maximum (map snd tmp) : cost [x] xs

main :: IO ()
main = do
    _ <- getLine
    as <- map read . words <$> getLine
    ws <- map read . words <$> getLine
    print $ moveIt (sort (zip as ws))