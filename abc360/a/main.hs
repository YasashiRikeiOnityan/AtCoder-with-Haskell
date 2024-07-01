module Main where

rms :: String -> String
rms "RMS" = "Yes"
rms "RSM" = "Yes"
rms "SRM" = "Yes"
rms _     = "No"

main :: IO ()
main = putStrLn . rms  =<< getLine