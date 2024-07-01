module Main where

import Data.List.Split ( chunksOf )

solve :: Int -> String -> String -> String
solve k s t 
    | k == pred (length s) = "No"
    | otherwise = if verticalReading t (chunksOf k s) then "Yes" else solve (succ k) s t

verticalReading :: String -> [String] -> Bool
verticalReading t ss = t `elem` vers ss
    where
        vers xs
            | all null xs = []
            | otherwise   = map head xs : vers (filter (not . null) (map tail  xs))

main :: IO ()
main = do
    [s, t] <- words <$> getLine
    putStrLn $ solve 1 s t
