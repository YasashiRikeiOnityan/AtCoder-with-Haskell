module Main where

p :: Int
p = 998244353

newtype IntMod = IntMod Int deriving (Eq, Show, Read)


main :: IO ()
main = do
    k <- readLn :: IO Int
    cs <- map read . words <$> getLine :: IO [Int]
    print cs
