module Problem19 where

-- import qualified Data.Matrix as Matrix
import qualified Utils as U

data Point = Point {
      x :: Int
    , y :: Int
    , z :: Int
} deriving (Show)

data Scanner = Scanner {
      number :: Int
    , beacons :: [Point]
} deriving (Show)

parseInput :: [String] -> [Scanner]
parseInput [] = []
parseInput (x:xs) = let scannerNumber = parseTitle x
                        input = takeWhile notTitle xs
                        beacons = map parseBeacon input
                        scanner = Scanner scannerNumber beacons
                        rest = dropWhile notTitle xs
                    in scanner:(parseInput rest)

parseTitle :: String -> Int
parseTitle title = read $ vals !! 2
               where vals = words title

notTitle :: String -> Bool
notTitle [] = True
notTitle xs | take 3 xs == "---" = False
            | otherwise = True

parseBeacon :: String -> Point
parseBeacon str = let (x:y:z:rest) = U.split ',' str
                  in Point (read x) (read y) (read z)


