module Utils where

split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
  (ls, "") -> [ls]
  (ls, x:rs) -> ls : split c rs

binToInt :: [Int] -> Int
binToInt [] = 0
binToInt (x : xs) = x + 2 * binToInt xs
