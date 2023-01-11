module Problem13 where

import qualified Utils as U
import qualified Data.List as L

data Fold = X Int | Y Int
        deriving (Show)
type Point = (Int, Int)

run :: String -> String
run input = let (points, folds) = parseInput input 
                nRows = maximum $ map fst points
                nCols = maximum $ map snd points
                grid = buildGrid (nRows + 1) (nCols + 1)
            in unlines $ foldl (\acc f -> runFold f acc) (placePoints points grid) folds

countVisible :: [String] -> Int
countVisible = length . filter (== '#') . concat

runFold :: Fold -> [String] -> [String]
runFold (X x) grid = L.transpose $ foldUp x (L.transpose grid)
runFold (Y y) grid = foldUp y grid

foldUp :: Int -> [String] -> [String]
foldUp line grid = let t = take line grid
                       b = reverse $ drop (line + 1) grid
                       (top, bottom) = rectifyLengths t b
                   in zipWith (\m n -> zipWith eitherDot m n) top bottom

rectifyLengths :: [String] -> [String] -> ([String], [String])
rectifyLengths x y | length x < length y = (pad x y, y)
                   | length x > length y = (x, pad y x)
                   | otherwise = (x, y)

pad :: [String] -> [String] -> [String]
pad shorter longer = let diff = (length longer) - (length shorter)
                         width = length $ head shorter
                         padding = take width $ repeat '.'
                     in (take diff $ repeat padding) ++ shorter

eitherDot :: Char -> Char -> Char
eitherDot '#' _ = '#'
eitherDot _ '#' = '#'
eitherDot _ _ = '.'

placePoints :: [Point] -> [String] -> [String]
placePoints [] grid = grid
placePoints ((x,y):ps) grid = let rowsAbove = take y grid
                                  row = grid !! y
                                  rowsBelow = drop (y + 1) grid
                                  newGrid = rowsAbove ++ [transform row] ++ rowsBelow
                                  transform r = (colsAhead r) ++ ['#'] ++ (colsBehind r)
                                  colsAhead a = take x a
                                  colsBehind a = drop (x + 1) a
                              in placePoints ps newGrid

buildGrid :: Int -> Int -> [String]
buildGrid x y = take y $ repeat row
            where row = take x $ repeat '.'


parseInput :: String -> ([Point], [Fold])
parseInput s = let l = lines s
                   points = takeWhile isPoints l
                   folds = tail $ dropWhile isPoints l
               in (map parsePoint points, map parseFold folds) 

isPoints :: String -> Bool
isPoints [] = False
isPoints xs = (`elem` ['0'..'9']) . head $ xs

parsePoint :: String -> Point
parsePoint s = let coords = U.split ',' s
               in (read $ head coords, read $ coords !! 1)

parseFold :: String -> Fold
parseFold s = let line = head $ reverse $ words s
                  parts = U.split '=' line
              in case (head parts) of
                    "x" -> X (read $ parts !! 1)
                    "y" -> Y (read $ parts !! 1)
