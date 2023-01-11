module Main where

import System.IO
import qualified Data.Map as M
import qualified Data.Maybe as Y

import qualified Problem7 as P7
import qualified Problem8 as P8
import qualified Problem9 as P9
import qualified Problem13 as P13
import qualified Problem14 as P14
import qualified Problem20 as P20

main :: IO ()
main = do
    putStrLn "What day would you like to run?"
    day <- getLine
    putStrLn "Do you want to (T)est or (R)un the code?"
    inputType <- getLine
    let fileName = getFileName day inputType
    (input, handle) <- getInputData fileName
    let problem = Y.fromJust $ M.lookup day problems
    let output = problem input
    putStr output
    hClose handle


getFileName :: String -> String -> String
getFileName day "T" = "data/" ++ day ++ "/test.txt"
getFileName day "R" = "data/" ++ day ++ "/input.txt"
getFileName _ i = error "no input file found for option " ++ i


getInputData :: String -> IO (String, Handle)
getInputData fileName = do
                         handle <- openFile fileName ReadMode
                         contents <- hGetContents handle
                         return (contents, handle)

problems :: M.Map String (String -> String)
problems = M.fromList [("7", P7.run)
                      ,("8", show . P8.run)
                      ,("9", show . P9.run)
                      ,("13", P13.run)
                      ,("14", show . P14.run)
                      ,("20", P20.run)]
