module Main where

import qualified Data.ByteString.Char8 as B
import Data.Array ( Array, listArray, (!), range )
import Data.List ( foldl', scanl' )

editDistance :: B.ByteString -> B.ByteString -> Int
editDistance xs ys = table ! (m, n)
    where
    (m, n) = (B.length xs, B.length ys)

    x = listArray (1, m) (B.unpack xs)
    y = listArray (1, n) (B.unpack ys)
    
    table :: Array (Int, Int) Int
    table = listArray bnds [dist ij | ij <- range bnds]
    bnds  = ((0, 0), (m, n))
    
    dist (0, j) = j
    dist (i, 0) = i
    dist (i, j) = minimum [
            table ! (i-1, j) + 1, 
            table ! (i ,j-1) + 1,
            if x ! i == y ! j then table ! (i-1, j-1) else 1 + table ! (i-1, j-1)
        ]

editDistance' :: String -> String -> Int
editDistance' s t = last . foldl' phi [0..length t] . zip [1..] $ s
    where
        phi acc (i, a) = scanl' (step a) i $ zip3 t acc (tail acc)
        step a i (b, j, k) = minimum [succ i, (if a == b then succ else id) j, succ k]
 
main :: IO ()
main = getLine >>= \s -> getLine >>= \t -> print $ editDistance' s t