module Main where

import Data.Bifunctor ( bimap )

lcs :: [Char] -> [Char] -> ([Char], Int)
lcs s t = head $ foldr phi (replicate (succ (length s)) ([], 0)) t
    where
        phi y row = foldr (step y) [([], 0)] (zip3 s row (tail row))
        step y (x, cs1, cs2) row
            | x == y    = bimap (y:) (1+) cs2 : row
            | otherwise = (if snd cs1 > snd (head row) then cs1 else head row) : row

main :: IO ()
main = getLine >>= \s -> getLine >>= \t -> print (snd (lcs s t))


phi y s row = foldr (step y) [([], 0)] (zip3 s row (tail row))
step y (x, cs1, cs2) row
    | x == y    = bimap (y:) (1+) cs2 : row
    | otherwise = (if snd cs1 > snd (head row) then cs1 else head row) : row
{-
ghci> phi 'o' "tokyo" (replicate 6 ("", 0))
[("o",1),("o",1),("o",1),("o",1),("o",1),("",0)]

ghci> phi 't' "tokyo" [("o",1),("o",1),("o",1),("o",1),("o",1),("",0)]
[("to",2),("o",1),("o",1),("o",1),("o",1),("",0)]

ghci> phi 'o' "tokyo" [("to",2),("o",1),("o",1),("o",1),("o",1),("",0)]
[("oo",2),("oo",2),("o",1),("o",1),("o",1),("",0)]

ghci> phi 'y' "tokyo" [("oo",2),("oo",2),("o",1),("o",1),("o",1),("",0)]
[("yo",2),("yo",2),("yo",2),("yo",2),("o",1),("",0)]

ghci> phi 'k' "tokyo" [("yo",2),("yo",2),("yo",2),("yo",2),("o",1),("",0)]
[("kyo",3),("kyo",3),("kyo",3),("yo",2),("o",1),("",0)]
-}