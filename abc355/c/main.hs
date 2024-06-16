module Main where

import Data.Array ( Array, (!), (//), listArray )

-- 関数(//)がO(N)であるためTLE
succElem :: Int -> Int -> Array Int Int -> (Array Int Int, Bool)
succElem n t arr = (arr // [(t, succ (arr ! t))], n == succ (arr ! t))

succDiag1 :: Int -> Int -> Int -> Int -> (Int, Bool)
succDiag1 n x y diag
    | x == y = (succ diag, succ diag == n)
    | otherwise = (diag, diag == n)

succDiag2 :: Int -> Int -> Int -> Int -> (Int, Bool)
succDiag2 n x y diag
    | x + y == pred n = (succ diag, succ diag == n)
    | otherwise = (diag, diag == n)

bingo :: Int -> Int -> [Int] -> (Array Int Int, Array Int Int, Int, Int) -> Int
bingo _ _ [] _ = -1
bingo i n (k : ks) (rows, cols, diag1, diag2) = if snd rows' || snd cols' || snd diag1' || snd diag2' then i else bingo (succ i) n ks (fst rows', fst cols', fst diag1', fst diag2') 
    where x = if k `mod` n == 0 then k `div` n - 1 else k `div` n
          y = if k `mod` n == 0 then pred n else k `mod` n - 1
          rows' = succElem n x rows
          cols' = succElem n y cols
          diag1' = succDiag1 n x y diag1
          diag2' = succDiag2 n x y diag2

main :: IO ()
main = do
    [n, _] <- map read . words <$> getLine
    as <- map read . words <$> getLine
    print $ bingo 1 n as (listArray (0, pred n) [0 | _ <- [1..n]], listArray (0, pred n) [0 | _ <- [1..n]], 0, 0)
