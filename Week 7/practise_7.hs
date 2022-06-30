-- Exercise 1
-- Write 3 definitions for a function called fib n evaluating the
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

-- Exercise 2
-- Write a function that returns the sum of the natural numbers up to n.

-- Using conditionals:
sumNat :: Int -> Int
sumNat n = if n == 0 then 0 else if n > 0 then n + sumNat (n - 1) else error "Negative number!"

-- Using guards:
sumNat1 :: Int -> Int
sumNat1 n
    | n == 0 = 0
    | n > 0 = n + sumNat (n - 1)
    | otherwise = error "Negative number!"

-- Exercise 3
-- Write a function before that checks whether time t1, given as a  tuple (h1, m1, s1) comes before
-- time t2 also give as a tuple (h2, m2, s2), returning true or false.
before :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
before (h1, m1, s1) (h2, m2, s2)
    | h1 < h2 = True
    | m1 < m2 = True
    | s1 < s2 = True
    | otherwise = False

before1 :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
before1 (h1, m1, s1) (h2, m2, s2)
    | h1 < h2 = True
    | h1 > h2 = False
    | m1 < m2 = True
    | m1 > m2 = False
    | otherwise = s1 < s2

-- Exercise 3
-- Write a function absVal that returns the absolute value of the value given to it as 
-- an arguement.
absVal :: Double -> Double
absVal x
    | x > 0 = x
    | x == 0 = 0
    | otherwise = x * (-1)

absVal1 :: Double -> Double
absVal1 x
    | x >= 0 = x
    | otherwise = -x

-- Exercise 4
-- Write a function eDist that computes the Euclidean distance between two coordinates (x1, y1)
-- and (x2, y2) in a 2-dimensional plane.
sqr :: Float -> Float
sqr n = n * n

eDist :: (Float, Float) -> (Float, Float) -> Float
eDist (x1, y1) (x2, y2) = sqrt(sqr dx + sqr dy)
    where dx = x2 - x1; dy = y2 - y1

eDist1 :: (Float, Float) -> (Float, Float) -> Float
eDist1 (x1, y1) (x2, y2) = sqrt (dx * dx + dy * dy)
    where
        dx = x2 - x1;
        dy = y2 - y1

-- Exercise 5
-- Write a function that returns the largest element of a non-empty list of integers.
-- If the list is empty, the function should display an error message.
largest :: [Int] -> Int
largest [] = error "Empty list!"
largest (x:xs) = if null xs || (x > x1) then x else x1
    where x1 = largest xs