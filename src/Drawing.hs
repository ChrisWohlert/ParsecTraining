module Drawing where


import Graphics.Gloss
import Class


memberHeight = 200

memberFullHeight = memberHeight + 10

marginLeft = 100
marginTop = 300

classWidth = 1000

classHeight c = fromIntegral (length (class_members c)) * memberFullHeight + 800

class Drawing a where
    draw :: a -> Picture


instance Drawing Solution where
    draw (Solution types) = translate (-1900) 0 $ pictures $ map (\ (t, x) -> translate x 400 $ draw t) $ zip types ([0, 300..4000])

instance Drawing Type where
    draw c@(Class _ _ _ _ _ _ name _ _ members _) = scale 0.2 0.2 $ pictures [ Color (makeColorI 0 0 0 80) $ translate (classWidth / 2) (-classHeight c / 2) $ rectangleSolid classWidth (classHeight c)
                                                             , (alignLeft . alignTop . draw) name
                                                             , (alignLeft . fromTop (marginTop + 400) . spreadVertical (-memberFullHeight)) members]
    draw _ = pictures [rectangleSolid 100 300]


instance Drawing ClassName where
    draw (ClassName name) = Text name
    draw (GenericClassName name t) = Text $ name ++ "<" ++ t ++ ">"

instance Drawing MethodName where
    draw (MethodName name) = Text name
    draw (GenericMethodName name t) = Text $ name ++ "<" ++ t ++ ">"

instance Drawing Member where
    draw (Method (Concrete methodSignature _)) = getNameFromSignature methodSignature
    draw (Method (Abstract methodSignature)) = getNameFromSignature methodSignature
    draw (Method (Interface methodSignature)) = getNameFromSignature methodSignature
    draw (Method (External methodSignature)) = getNameFromSignature methodSignature
    draw (Method (ArrowFunction methodSignature _)) = getNameFromSignature methodSignature
    draw (Method (OperatorOverload _ _ _ parameters _ _ _)) = Text "Name"
    draw _ = Text "Name"

getNameFromSignature (MethodSignature vis static modifier returnType name parameters _ _) = draw name

spreadVertical :: Drawing a => Float -> [a] -> Picture
spreadVertical d items = pictures $ map (\ (i, x) -> translate 0 x $ draw i) $ zip items ([0, d..])

alignLeft = translate marginLeft 0

alignTop = fromTop marginTop

fromTop d = translate 0 (-d)