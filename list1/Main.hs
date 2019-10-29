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
oneway [] = [[]]
oneway xs = xs:oneway(tail xs)

otherway xs []