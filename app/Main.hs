module Main (main) where

import Lib
import System.IO

main :: IO ()
main = do
    handle <- openFile "test-data/SomeClass.cs" ReadMode  
    contents <- hGetContents handle
    case run_parseType contents "SomeClass" of
        Left err -> print err
        Right c -> print $ contents ++ (show c)
    hClose handle 
