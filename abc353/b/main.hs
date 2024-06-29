module Main where

atraction :: Int -> Int -> [Int] -> Int
atraction _ _ [] = 1
atraction w k aas@(a : as)
    | w + a > k  = succ $ atraction 0 k aas
    | otherwise  = atraction (w + a) k as

main :: IO ()
main = do
    [_, k] <- map read . words <$> getLine :: IO [Int]
    as <- map read . words <$> getLine :: IO [Int]
    print . atraction 0 k $ as
