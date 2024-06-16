module Main where

type Vect a = ([a], Int)

nil :: Vect a
nil = ([], 0)

cons :: a -> Vect a -> Vect a
cons x (xs, n) = (x : xs, succ n)

size :: Vect a -> Int
size = snd

maxVect :: Vect a -> Vect a -> Vect a
maxVect xs@(_, m) ys@(_, n)
    | m > n     = xs
    | otherwise = ys

lcs :: Eq a => [a] -> [a] -> Vect a
lcs s t = head $ foldr phi (replicate (succ (length s)) nil) t
    where
        phi y row = foldr (step y) [nil] (zip3 s row (tail row))
        step y (x, cs1, cs2) row
            | x == y    = cons x cs2 : row
            | otherwise = maxVect cs1 (head row) : row

main :: IO ()
main = getLine >>= \s -> getLine >>= \t -> print (size (lcs s t))
