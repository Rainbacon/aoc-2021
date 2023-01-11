module Problem20 where

import Debug.Trace (trace)

import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Utils as U

type Point = (Int, Int)
type Image = Map.Map Point Char

run :: String -> String
run input = let (algo, image) = parseInput input
                points = Map.keys image
                extraPoints = points List.\\ (extend length width)
                length = maximum $ map fst points
                width = maximum $ map snd points
                initialState = bulkInsert extraPoints image
            in show $ State.evalState (transformImage 2 algo points) initialState

parseInput :: String -> (String, Image)
parseInput input = let (algo:blank:img) = lines input
                       rows = zip [0..] img
                       cols = map (\(x,row) -> zipWith (\y c -> ((x,y), c)) [0..] row) rows
                       image = Map.fromList $ concat cols
                   in (algo, image)

transformImage :: Int -> String -> [Point] -> State.State Image Int
transformImage 0 algo points = do
                                image <- State.get
                                return $ sumLit image points
transformImage n algo points = do
                                image <- State.get
                                let newImage =  Map.mapWithKey (transformPixel algo image) image
                                State.put $ trace (show newImage) newImage
                                transformImage (n - 1) algo points
                                

sumLit :: Image -> [Point] -> Int
sumLit _ [] = 0
sumLit i (p:ps) = case Map.lookup p i of
                    Just '#' -> 1 + sumLit i ps
                    otherwise -> sumLit i ps

charToDigit :: Char -> Int
charToDigit '#' = 1
charToDigit _ = 0

adjacents :: Point -> [Point]
adjacents (x, y) = [
                    (x - 1, y - 1), (x - 1, y), (x - 1, y + 1),
                    (x, y - 1), (x, y), (x, y + 1),
                    (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)
                   ]

extend :: Int -> Int -> [Point]
extend length width = let xs = map (,) [-3..length + 3]
                          ys = [-3..width + 3]
                      in xs <*> ys

bulkInsert :: [Point] -> Image -> Image
bulkInsert [] i = i
bulkInsert (p:ps) i = Map.insert p '.' $ bulkInsert ps i

transformPixel :: String -> Image -> Point -> Char -> Char
transformPixel algo image point _ = let points = adjacents point
                                        digits = map (\p -> charToDigit $ Map.findWithDefault '.' p image) points
                                    in algo !! (U.binToInt digits)

