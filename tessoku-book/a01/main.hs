module Main where

main :: IO ()
main = do
    n <- readLn
    print $ n * n
