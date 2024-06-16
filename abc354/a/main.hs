module Main where

vs :: (Int, Int) -> Int -> Int
vs (i, h) h'
    | h + 2 ^ i > h' = succ i
    | otherwise = vs (succ i, h + 2 ^ i) h'

main :: IO ()
main = print . vs (0, 0) =<< readLn
