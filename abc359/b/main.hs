module Main where

count :: [Int] -> Int
count (x : y : z : zs)
    | x == z    = succ . count $ y : z : zs
    | otherwise = count $ y : z : zs
count _ = 0

main :: IO ()
main = do
    _ <- getLine
    as <- map read . words <$> getLine
    print . count $ as
