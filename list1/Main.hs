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

fib n
    | n == 0 = 1
    | otherwise = fib' n 1 0 where
        fib' n result previous
            | n == 0 = result
            | otherwise = fib' (n-1) (result+previous) result

-- Exrecise 4

-- a
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1
totient n = length [x | x <- [1..n], coprime x n]

-- b
totient_sum n = sum (map totient [x | x <- [1..n], n `rem` x == 0])
totient_sum' n = n

-- Exercise 5
oneway xs = [take n xs | n <- [0.. length xs]]

otherway [] = [[]]
otherway xs = xs:otherway( tail xs)

partition xs = zip (oneway xs) (otherway xs)

-- Exercise 6
remDup :: Eq a => [a] -> [a]
remDup xs = reverse (remDup' xs []) where
    remDup' [] acc = acc
    remDup' (x:xs) [] = remDup' xs (x:[])
    remDup' (x:xs) acc = 
        if x == head acc then remDup' xs acc
        else remDup' xs (x:acc)

-- Exercise 7
listDup :: Eq a => [a] -> [[a]]
listDup xs = reverse( lDup' xs [] [] ) where
    lDup' [] acc1 [] = acc1
    lDup' [] acc1 acc2 = acc2:acc1
    lDup' (x:xs) acc [] = lDup' xs acc [x]
    lDup' (x:xs) acc1 acc2 = 
        if x == head acc2 then lDup' xs acc1 (x:acc2)
        else lDup' xs (acc2:acc1) [x]

-- Exercise 8
cco :: String -> [(Char, Integer)]
cco "" = []
cco (x:xs) = reverse (cco' xs [] (x, 1)) where
    cco' [] acc (a, n) = ((a, n):acc)
    cco' (x:xs) acc (a, n) =
        if x == a then cco' xs acc (a, n+1)
        else cco' xs ((a, n):acc) (x,1)

dcco :: [(Char, Integer)] -> String
dcco [] = []
dcco (x:xs) = [fst x | _ <- [1..snd x]] ++ dcco xs
-- to poniżej też chyba powinno działać
-- dcco (x:xs) = take (snd x ) repeat(fst x) ++ dcco xs


-- Exercise 9
powerlist :: [a] -> [[a]]
powerlist []  = [[]]
powerlist (x:xs) = powerlist xs ++ map (x:) (powerlist xs)

-- Exercise 10
perms [] = [[]]
perms xs = [ i:j | i <- xs, j <- perms $ delete i xs ] where
    delete a xs = [x | x <- xs, a /= x]
-- 