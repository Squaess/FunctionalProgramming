g x = x+2

badfib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

-- try to make better with tail recursion?

