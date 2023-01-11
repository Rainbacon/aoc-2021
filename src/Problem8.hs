module Problem8 where

import qualified Utils as U
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y

run :: String -> Int
run input = let parsed = extract input
            in sum $ map processSignal parsed

processSignal :: ([String], [String]) -> Int
processSignal (patterns, output) = let fir (a,b,c) = a
                                       sec (a,b,c) = b
                                       thd (a,b,c) = c
                                       sno = findSixNineZero patterns one four
                                       ttf = findTwoThreeFive patterns one nine
                                       zero = thd sno
                                       one = findOne patterns
                                       two = fir ttf
                                       three = sec ttf
                                       four = findFour patterns
                                       five = thd ttf
                                       six = fir sno
                                       seven = findSeven patterns
                                       eight = findEight patterns
                                       nine = sec sno
                                       converter = M.fromList $ zipWith (\n i -> (L.sort n, i)) [zero,one,two,three,four,five,six,seven,eight,nine] [0..9]
                                   in sum $ zipWith (*) [1000,100,10,1] $ map (\k -> Y.fromJust $ M.lookup (L.sort k) converter) output

                                    

countUniq :: [String] -> Int
countUniq = length . (filter (\x -> length x `elem` [2,3,4,7]))

extract :: String -> [([String], [String])]
extract = (map extractRow) . lines
     where extractRow row = let inOut = U.split '|' row
                                first = head inOut
                                second = inOut !! 1
                            in (words first, words second)

digits :: String -> [(String -> Bool)]
digits = map elem


allDigits :: String -> String -> Bool
allDigits number pattern = foldl (&&) True $ map (\f -> f pattern) (digits number)


findOne :: [String] -> String
findOne = head . (filter ((==2) . length))

findSeven :: [String] -> String
findSeven = head . (filter ((==3) . length))

findFour :: [String] -> String
findFour = head . (filter ((==4) . length))

findEight :: [String] -> String
findEight = head . (filter ((==7) . length))

findSixNineZero :: [String] -> String -> String -> (String, String, String)
findSixNineZero patterns one four = let sixNineZero = filter ((==6) . length) patterns
                                        six = head $ filter (\p -> not $ ((allDigits one p) || (allDigits four p))) sixNineZero
                                        nine = head $ filter (allDigits four) sixNineZero
                                        zero = head $ filter (\p -> (not $ (allDigits four p)) && allDigits one p) sixNineZero
                                    in (six, nine, zero)


findTwoThreeFive :: [String] -> String -> String -> (String, String, String)
findTwoThreeFive patterns one nine = let twoThreeFive = filter ((==5) . length) patterns
                                         three = head $ filter (allDigits one) twoThreeFive
                                         two = head $ filter (\x -> not $ x `elem` [three, five]) twoThreeFive
                                         five = head $ filter (\p -> (allDigits p nine) && (not $ allDigits one p)) twoThreeFive
                                     in (two, three, five)
