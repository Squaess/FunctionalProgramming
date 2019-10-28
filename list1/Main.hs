-- Exercise 1
fact :: Integer -> Integer
fact n = fact' n 1 where
    fact' :: Integer -> Integer -> Integer
    fact' 0 acc = acc
    fact' 1 acc = acc
    fact' n acc = fact' (n-1) (n*acc)

fact'' n = foldl (*) 1 [1..n]


-- Exercise 2
rev [] = []
rev (x:xs) = rev xs ++ [x]

rev' :: [a] -> [a]
rev' list = rev'' list [] where
    rev'' [] xs = xs
    rev'' (x:xs) s = rev'' xs (x:s)

rev''' :: [a] -> [a]
rev''' = foldl (\a x -> x:a) []


-- Exercise 3

fibAux n result previous
    | n == 0 = result
    | otherwise = fibAux (n-1) (result + previous) result

fibTail n
    | n == 0 = 0
    | otherwise = fibAux n 1 0
