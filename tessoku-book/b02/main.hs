module Main where

yn :: Bool -> String
yn b = if b then "Yes" else "No"

main :: IO ()
main = do
    [a, b] <- map read . words <$> getLine :: IO [Int]
    putStrLn . yn . not . null $ [x | x <- [a..b], 100 `mod` x == 0]
