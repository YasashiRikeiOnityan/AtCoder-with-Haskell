module Main where

import Data.Bifunctor ( bimap )

lcs :: [Char] -> [Char] -> ([Char], Int)
lcs s t = head $ foldr phi (replicate (succ (length s)) ("", 0)) t
    where
        phi y row = foldr (step y) [("", 0)] (zip3 s row (tail row))
        step y (x, cs1, cs2) row
            | x == y    = bimap (y:) (1+) cs2 : row
            | otherwise = (if snd cs1 > snd (head row) then cs1 else head row) : row

main :: IO ()
main = getLine >>= \s -> getLine >>= \t -> print (snd (lcs s t))
