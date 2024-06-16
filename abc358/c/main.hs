module Main where

import Control.Monad ( replicateM )
import Data.List ( nub )

minimumVisit :: Int -> [[Int]] -> [[Int]] -> Int
minimumVisit i ss tt
    | i == 0 = if any (notElem 0) ss then 1 else minimumVisit (succ i) ss tt
    | otherwise = if any (notElem 0) zipped then succ i else minimumVisit (succ i) ss zipped
        where zipped = nub [[if u + v >= 1 then 1 else 0 | (u, v) <- zip s t] | s <- ss, t <- tt, s /= t]

main :: IO ()
main = do
    [n, _] <- map read . words <$> getLine
    ss <- map (map (\c -> if c == 'o' then 1 else 0)) <$> replicateM n getLine
    print $ minimumVisit 0 ss ss
