module Main where

purchase :: Int -> Int -> [Int] -> [Int]
purchase _ _ [] = []
purchase k a (t : ts)
    | k < t = (t + a) : purchase (t + a) a ts
    | otherwise = (k + a) : purchase (k + a) a ts

main :: IO ()
main = do
    [_, a] <- map read . words <$> getLine
    ts <- map read . words <$> getLine
    mapM_ print (purchase 0 a ts) 
