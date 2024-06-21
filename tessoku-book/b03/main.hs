module Main where

just1000 :: [(Int, Int)] -> Bool
just1000 as 
    = 1000 `elem` [x + y + z | (i, x) <- as, (j, y) <- as, i /= j, (k, z) <- as, j /= k, k /= i]

yn :: Bool -> String
yn b = if b then "Yes" else "No"

main :: IO ()
main = do
    _ <- getLine
    as <- map read . words <$> getLine
    putStrLn . yn . just1000 . zip [1..] $ as
