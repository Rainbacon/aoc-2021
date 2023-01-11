module Problem7 (run) where

import qualified Utils as U
import qualified Data.Map as M
import Data.List (sort)

run :: String -> String
run input = let positions = map read $ U.split ',' input
            in show $ calculateFuel positions

calculateFuel :: [Int] -> Int
calculateFuel positions = let positionMap = foldl (\acc x -> M.insertWith (+) x 1 acc) M.empty positions
                              maxKey = head . reverse . sort . M.keys $ positionMap
                              things = map (\(k,v) target -> v * (tri $ abs (k - target))) $ M.assocs positionMap
                          in head $ sort $ map (\x -> sum $ map ($ x) things) [0..maxKey]

tri :: Int -> Int
tri n = sum [1..n]
