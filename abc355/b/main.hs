module Main where

import Data.List ( sort )

abc355b :: [Int] -> [Int] -> Bool -> String
abc355b [] _ _ = "No"
abc355b [_] [] bool = if bool then "Yes" else "No"
abc355b _ [] _ = "Yes"
abc355b (a : as) (b : bs) bool
    | bool && a < b = "Yes"
    | a < b = abc355b as (b : bs) True
    | otherwise = abc355b (a : as) bs False

main :: IO ()
main = do
    _ <- getLine
    as <- map read . words <$> getLine
    bs <- map read . words <$> getLine
    putStrLn $ abc355b (sort as) (sort bs) False
