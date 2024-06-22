module Main where

solve :: Integer -> Integer -> Integer -> Integer -> Integer
solve sx sy tx ty
    | odd (sx + sy) && odd (tx + ty)  = (abs (sy - ty) + max (abs (pred sx - pred tx)) (abs (sy - ty))) `div` 2 
    | odd (sx + sy) && even (tx + ty) = (abs (sy - ty) + max (abs (pred sx - tx)) (abs (sy - ty))) `div` 2 
    | even (sx + sy) && odd (tx + ty) = (abs (sy - ty) + max (abs (sx - pred tx)) (abs (sy - ty))) `div` 2 
    | otherwise                       = (abs (sy - ty) + max (abs (sx - tx)) (abs (sy - ty))) `div` 2 

main :: IO ()
main = do
    [sx, sy] <- map read . words <$> getLine
    [tx, ty] <- map read . words <$> getLine
    print $ solve sx sy tx ty
