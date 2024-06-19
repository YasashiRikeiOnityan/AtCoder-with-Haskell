module Main where

b03 :: [(Int, Int)] -> Bool
b03 as = 1000 `elem` [x + y + z | (i, x) <- as, (j, y) <- as, i /= j, (k, z) <- as, j /= k, k /= i]

yn :: Bool -> String
yn b = if b then "Yes" else "No"

main :: IO ()
main = do
    _ <- getLine
    as <- map read . words <$> getLine
    putStrLn . yn . b03 . zip [1..] $ as
