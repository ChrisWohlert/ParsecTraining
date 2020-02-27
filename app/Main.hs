module Main (main) where

import Lib
import System.IO
import qualified Graphics.UI.GLUT as GLUT
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Export.PNG
import Drawing
import Class

background :: Color
background = white

render = pure . draw

main :: IO ()
main = do
    (_, _) <- GLUT.getArgsAndInitialize
    types <- parseFiles "C:/Users/CWO/source/github/ParsecTraining"
    picture <- render (Solution types)
    exportPictureToPNG (4000, 4000) background "C:\\Users\\CWO\\Desktop\\pic.png" picture