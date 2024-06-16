module Main where

atcoder :: String
atcoder = "AtCoder"

land :: String
land = "Land"

main :: IO ()
main = do
    [s, t] <- words <$> getLine
    putStrLn $ if s == atcoder && t == land then "Yes" else "No"
