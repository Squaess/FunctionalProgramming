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