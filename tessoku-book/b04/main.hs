module Main where

binary2int :: String -> Int
binary2int [] = 0
binary2int (x : xs) = read [x] * 2 ^ length xs + binary2int xs

main :: IO ()
main = do
    s <- getLine
    print . binary2int $ s
