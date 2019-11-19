-- Exercise 1: Express map via foldr. Use lambda expressions

-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f(x):y) []

-- Task 2: Implement a function that for a given list of integers returns the sum of squares
-- of its even members. Use fold.

t2 :: (Integral a) => [a] -> a
t2 [] = 0
t2 xs = foldl (\x y -> x + y^2) 0 $ filter (\x -> x `mod` 2 == 0) xs

-- Task 3: Implement a function that for a given list of natural numbers
-- calculates how many members of the list are prime. It should be reasonably fast. Use fold.

prime :: (Integral a) => a -> Bool
prime 1 = True
prime x = foldr (&&) True [ x `mod` y /= 0 | y <- [2..(x-1)] ]

primeCount :: [Int] -> Int
primeCount [] = 0
primeCount xs = length $ filter prime xs

-- Task 4: Implement a function that for a given natural 
-- number n calculates the approximation of e, i.e. ∑^{n}_{k=0}1/k!. It should do so fast - in a linear time.

f :: (Double, Double) -> Int -> (Double, Double)
f x next = ( fst x + (1/next_fac), next_fac) where
    next_fac = snd x * (fromIntegral next)

task3 :: Int -> Double
task3 x = fst (foldl f (1.0, 1.0) [1..x] )