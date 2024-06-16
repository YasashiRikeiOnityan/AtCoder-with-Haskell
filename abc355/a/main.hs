module Main where

import Data.List ( (\\) )

abc355a :: [Int] -> Int
abc355a xs = if length rest == 1 then head rest else -1
    where rest = [1..3] \\ xs

main :: IO ()
main = do
    ab <- map read . words <$> getLine
    print . abc355a $ ab
