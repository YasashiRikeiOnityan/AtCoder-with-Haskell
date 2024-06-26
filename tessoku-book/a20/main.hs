module Main where

import Data.Bifunctor ( bimap )

lcs :: String -> String -> (String, Int)
lcs s t = head $ foldr phi (replicate (succ (length s)) ("", 0)) t
    where
        phi y acc = foldr (step y) [("", 0)] (zip3 s acc (tail acc))
        step _ _ [] = undefined
        step y (x, cs1, cs2) acc@(a : _)
            | x == y    = bimap (y:) (+1) cs2 : acc
            | otherwise = (if snd cs1 > snd a then cs1 else a) : acc

main :: IO ()
main = getLine >>= \s -> getLine >>= \t -> print . snd $ lcs s t