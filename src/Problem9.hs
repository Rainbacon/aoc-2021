module Problem9 where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)

run :: String -> Int
run inp = let m = parseInput inp
              basinsRaw = map (\v -> bfs [fst v] [] m) (valleys m)
              basins = map (S.fromList . fst) basinsRaw
          in product $ take 3 $ reverse $ L.sort $ map S.size basins

parseInput :: String -> M.Map Coord Int 
parseInput input = let rows = zip [0..] $ lines input
                   in foldl insertPoints M.empty rows

insertPoints :: M.Map Coord Int -> (Int, String) -> M.Map Coord Int
insertPoints m (x, row) = foldl (\acc (y, v) -> M.insert (x, y) (C.digitToInt v) acc) m cols
                      where cols = zip [0..] row

valleys :: M.Map Coord Int -> [(Coord, Int)]
valleys m = M.assocs $ M.filterWithKey (isLowest m) m

isLowest :: M.Map Coord Int -> Coord -> Int -> Bool
isLowest m coord val = let neighbors = map ($ coord) [up, down, left, right]
                           isLower v n = case n of
                                            Nothing -> True
                                            Just w -> v < w
                       in foldl (\acc n -> acc && (isLower val n)) True $ map (\k -> M.lookup k m) neighbors

up :: Coord -> Coord
up (x, y) = (x, y - 1)

down :: Coord -> Coord
down (x, y) = (x, y + 1)

left :: Coord -> Coord
left (x, y) = (x - 1, y)

right :: Coord -> Coord
right (x, y) = (x + 1, y)

bfs :: [Coord] -> [Coord] -> M.Map Coord Int -> ([Coord], [Coord])
bfs [] v m = ([], v)
bfs (x:xs) v m = case M.lookup x m of
                    Nothing -> bfs xs v m
                    Just 9 -> bfs xs (x:v) m
                    Just n -> let neighbors = map ($ x) [up, down, left, right]
                                  unvisited = filter (\a -> not $ a `elem` v) neighbors
                                  (basin, visited) = bfs (xs ++ unvisited) (x:v) m
                              in (x:basin, visited)
