module Main where

import Data.Bifunctor ( bimap )
import Data.Array ( Ix(range), Array, (!), listArray )
import Data.List ( foldl', scanl' )

lcs :: String -> String -> Int
lcs s t = snd . head $ foldr phi (replicate (succ (length s)) ("", 0)) t
    where
        phi y acc = foldr (step y) [("", 0)] (zip3 s acc (tail acc))
        step _ _ [] = undefined
        step y (x, cs1, cs2) acc@(a : _)
            | x == y    = bimap (y:) (+1) cs2 : acc
            | otherwise = (if snd cs1 > snd a then cs1 else a) : acc

lcs' :: String -> String -> Int
lcs' xs ys = table ! (m,n)
    where
        (m, n) = (length xs, length ys)

        x = listArray (1, m) xs
        y = listArray (1, n) ys

        table :: Array (Int,Int) Int
        table = listArray bnds [dist ij | ij <- range bnds]
        bnds  = ((0, 0), (m, n))
        
        dist (0, 0) = 0
        dist (0, j) = table ! (0, pred j)
        dist (i, 0) = table ! (pred i, 0)
        dist (i, j) = maximum [
                table ! (pred i, j), 
                table ! (i, pred j),
                if x ! i == y ! j then succ (table ! (pred i, pred j)) else table ! (pred i, pred j)
            ]

lcs'' :: String -> String -> Int
lcs'' s t = last . foldl' phi (replicate (succ (length t)) 0) . map (0,) $ s
    where
        phi acc (i, a) = scanl' (step a) i $ zip3 t acc (tail acc)
        step a i (b, j, k) = maximum [i, (if a == b then succ else id) j, k]

main :: IO ()
main = getLine >>= \s -> getLine >>= \t -> print $ lcs'' s t