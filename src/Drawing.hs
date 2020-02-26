module Drawing where


import Graphics.Gloss
import Class






class Drawing a where
    draw :: a -> Picture



instance Drawing Class where
    draw c = circle 80