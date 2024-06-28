module Main where

import qualified Data.ByteString.Char8 as B
import Data.Array ( Array, array, (!) ,range )

editDistance :: B.ByteString -> B.ByteString -> Int
editDistance xs ys = table ! (m,n)
    where
    (m,n) = (B.length xs, B.length ys)
    x     = array (1,m) (zip [1..] (B.unpack xs))
    y     = array (1,n) (zip [1..] (B.unpack ys))
    
    table :: Array (Int,Int) Int
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds  = ((0,0),(m,n))
    
    dist (0,j) = j
    dist (i,0) = i
    dist (i,j) = minimum [table ! (i-1,j) + 1, table ! (i,j-1) + 1,
        if x ! i == y ! j then table ! (i-1,j-1) else 1 + table ! (i-1,j-1)]

main :: IO ()
main = B.getLine >>= \s -> B.getLine >>= \t -> print $ editDistance s t
{-
module Main where


import qualified Data.ByteString.Char8 as B
import Data.Array ( Array, listArray, (!) ,range )

editDistance :: B.ByteString -> B.ByteString -> Int
editDistance xs ys = table ! (m, n)
    where
        (m, n) = (B.length xs, B.length ys)
        x      = listArray (1, m) (B.unpack xs)
        y      = listArray (1, n) (B.unpack ys)

        table :: Array (Int, Int) Int
        table = listArray bnds [dist ij | ij <- range bnds]
        bnds  = ((0, 0), (m, n))

        dist (0, j) = j
        dist (i, 0) = i
        dist (i, j) = minimum [succ (table ! (pred i, j)), succ (table ! (i, pred j)),
            if x ! i == y ! j then table ! (pred i, pred j) else succ (table ! (pred i, pred j))]

main :: IO ()
main = B.getLine >>= \s -> B.getLine >>= \t -> print $ editDistance s t
-}
