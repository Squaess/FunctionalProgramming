doubleMe x = x + x
doubleAs x y = x*2 + y*2
doubleSmallNumber x = if x > 100
                        then x
                        else x*2


lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"  

signumA :: Double -> Int
signumA 0 = 0
signumA a = if a > 0
    then 1
    else if a < 0
    then -1
    else 0

signumA' :: Double -> Int
signumA' a
    | a > 0     = 1
    | a < 0     = -1
    | otherwise = 0

tell_bmi :: Double -> Double -> String
-- tell_bmi h w = if w/(h^2)<=18.5 then "you are underweight" else if w/(h^2)<25 then "you have normal weight" else "you are overweight"
tell_bmi h w
    = let bmi = w/(h^2)
    in case () of
        _ | bmi <= skinny  -> s ++ " underweight"
          | bmi < normal  -> s ++ " normal"
          | otherwise       -> s ++ " overweght"
    where
        s = "you are"
        skinny = 18.5
        normal = 25


if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y


dupa d =
    let z = x*x
    in f d
        |d > z = "dupa"
        |otherwise = "cipa"