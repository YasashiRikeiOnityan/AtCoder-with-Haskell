module Main where

yn :: Bool -> String
yn b = if b then "Yes" else "No"

main :: IO ()
main = do
    [_, x] <- map read . words <$> getLine :: IO [Int]
    as <- map read . words <$> getLine :: IO [Int]
    putStrLn . yn . elem x $ as
