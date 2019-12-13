data Point = Point Float Float
data Shape = Circle Point Float | Ractangle Point Point

calcSurf :: Shape -> Float
calcSurf (Circle _ x) = pi * x^2
calcSurf (Ractangle (Point x1 y1) (Point x2 y2)) = abs (x1-x2) * abs (y2-y1)

data IntOrString = Word String | Number Int

instance Show IntOrString where
    show (Word a)  = show a
    show (Number a) = show a
