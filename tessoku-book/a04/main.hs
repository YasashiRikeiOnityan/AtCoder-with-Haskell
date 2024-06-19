module Main where

-- int2binary :: Int -> [Char]
-- int2binary 0 = ['0']
-- int2binary 1 = ['1']
-- int2binary n = (if even n then '0' else '1') : int2binary (n `div` 2)

int2binary :: Int -> String
int2binary 0 = "0"
int2binary 1 = "1"
int2binary n = int2binary (n `div` 2) ++ show (n `mod` 2)

assignZero :: String -> String
assignZero s = replicate (10 - length s) '0' ++ s

main :: IO ()
main = do
    n <- readLn
    putStrLn . assignZero . int2binary $ n
