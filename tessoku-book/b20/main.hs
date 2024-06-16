module Main where

type EDProblem = (String, String)
type Distance = Int

editDistance :: EDProblem -> Distance
editDistance ([], []) = 0
editDistance (x : xs, []) = 1 + editDistance (xs, [])
editDistance ([], y : ys) = 1 + editDistance ([], ys)
editDistance (x : xs, y : ys) = 
    minimum [
        s + editDistance (xs, ys),
        1 + editDistance (x : xs, ys),
        1 + editDistance (xs, y : ys)
    ] where s = if x == y then 0 else 1

pair :: [a] -> (a, a)
pair [x, y] = (x, y)
pair _ = (undefined, undefined)

main :: IO ()
main = do
    s <- getLine
    t <- getLine
    print $ editDistance (s, t)