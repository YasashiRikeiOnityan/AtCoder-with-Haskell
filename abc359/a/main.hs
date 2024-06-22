module Main where

import Control.Monad ( replicateM )

takahashiNum :: [String] -> Int
takahashiNum [] = 0
takahashiNum ([] : ss) = takahashiNum ss
takahashiNum ((h : _) : ss)
    | h == 'T'  = 1 + takahashiNum ss
    | otherwise = takahashiNum ss

main :: IO ()
main = do
    n <- readLn
    ss <- replicateM n getLine
    print . takahashiNum $ ss
