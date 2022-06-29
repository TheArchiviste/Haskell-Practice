-- Exercise 1
-- Write 3 definition for a function called fib n evaluating the
-- n-th value of the Fibonnaci sequence.

-- Using conditionals:
fib1 :: Integer -> Integer
fib1 n = if n == 1 then 1 else if n == 2 then 1 else fib1(n - 2) + fib1(n - 1)

-- Using guards:
fib2 :: Integer -> Integer
fib2 n 
    | n == 1 = 1
    | n == 2 = 1
    | otherwise = fib2(n - 2) + fib2(n - 1)

-- Using pattern matching:
fib3 :: Integer -> Integer
fib3 1 = 1
fib3 2 = 1
fib3 n = fib3(n - 2) + fib3(n - 1)

-- Another solution to obtaining the fib function's values:
fib4 :: Int -> Int
fib4 n = xs !! (n - 1)
    where xs = 1 : 1 : zipWith (+) xs (tail xs)