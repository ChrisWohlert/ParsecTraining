module Main (main) where

import Lib
import System.IO
import Graphics.Gloss

window :: Display
window = InWindow "Depsgraph" (600, 400) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing