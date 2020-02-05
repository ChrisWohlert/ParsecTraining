module Main where

import Lib
import System.IO

main :: IO ()
main = do
    handle <- openFile "test-data/SomeCSV.csv" ReadMode  
    contents <- hGetContents handle
    case parseCSV contents of
        Left err -> print err
        Right r -> print r
    hClose handle 
