{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}

import System.Environment 
import System.IO 
import System.Random
import Data.List

formatArgs :: [String] -> Bool -> String
formatArgs [] _ = []
formatArgs (string:[]) _ = string
formatArgs (string:strings) True = string ++ "\n" ++ (formatArgs strings True)
formatArgs (string:strings) False = string ++ (formatArgs strings False)

main = do 
    args <- getArgs
    if not (null args) && head args == "-n" then
        putStrLn $ formatArgs (tail args) False else
        putStrLn $ formatArgs args True
    gen <- getStdGen
    putStrLn $ show $ lottery gen

{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
lottery :: StdGen -> [Int]
lottery gen = sort $ take 6 $ randomRs (1, 49) gen
