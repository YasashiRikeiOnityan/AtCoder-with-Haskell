module Main where

main :: IO ()
main = print . sum . map read . words =<< getLine
