module Problem14 where

import qualified Control.Monad.State as S
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Tuple as T

type Rule = (String, (String, String))
type PolymerState = M.Map String Integer
type Rules = M.Map String (String, String)

run :: String -> Integer
run input = let (start, rules) = parseInput $ lines input
            in S.evalState (runRules 40 rules) (toPairs start)

parseInput :: [String] -> (String, Rules)
parseInput (p:_:rs) = (p, M.fromList $ map toRule rs)

toRule :: String -> Rule
toRule (a:b:' ':'-':'>':' ':c:[]) = (a:b:[], (a:c:[], c:b:[]))

toPairs :: String -> PolymerState
toPairs [] = M.empty
toPairs (x:[]) = M.empty
toPairs xs = M.insertWith (+) key 1 m
         where key = take 2 xs
               m = toPairs $ tail xs

transformBase :: Rules -> String -> (String, String)
transformBase m s = Y.fromJust $ M.lookup s m

runRules :: Int -> Rules -> S.State PolymerState Integer
runRules 0 _ = do
                pairs <- S.get
                let frequencies = calcFrequencies pairs
                return (calcMinMax frequencies)
runRules n rules = do
                    pairs <- S.get
                    let newPairs = transformPairs pairs rules
                    S.put newPairs
                    runRules (n - 1) rules 
                
transformPairs :: PolymerState -> Rules -> PolymerState
transformPairs polymer rules = foldl insertBases M.empty pairs
                           where initialPairs = map T.swap $ M.assocs polymer
                                 transformer = fmap (transformBase rules)
                                 pairs = map transformer initialPairs

insertBases :: PolymerState -> (Integer, (String, String)) -> PolymerState
insertBases ps (n, (b1, b2)) = M.insertWith (+) b2 n $ M.insertWith (+) b1 n ps

calcFrequencies :: PolymerState -> M.Map Char Integer
calcFrequencies ps = foldl insertFreqs M.empty $ M.assocs ps

insertFreqs :: M.Map Char Integer -> (String, Integer) -> M.Map Char Integer
insertFreqs m (key, val) = foldl (\acc c -> M.insertWith (+) c val acc) m key

calcMinMax :: M.Map Char Integer -> Integer
calcMinMax m = let freqs = map (dedupe . snd) $ M.assocs m
                   max = head $ reverse $ L.sort freqs
                   min = head $ L.sort freqs
               in max - min

dedupe :: Integer -> Integer
dedupe n | odd n = n `div` 2 + 1
         | otherwise = n `div` 2
