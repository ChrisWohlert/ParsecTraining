module Main where

import Lib
import System.IO

main :: IO ()
main = do
    handle <- openFile "test-data/SomeClass.cs" ReadMode  
    contents <- hGetContents handle
    putStr (training contents)
    hClose handle 
