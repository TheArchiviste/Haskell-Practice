-- Exercise 1
-- Write a recursive function sumOdd to calculate the sum of the first n odd
-- numbers.
sumOdd :: Int -> Int
sumOdd n
    | n < 0 = error "Negative arguement!"
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = (2*n - 1) + sumOdd(n - 1)

sumOdd' :: Int -> Int
sumOdd' x = if x == 1 then 1 else (2*x - 1) + sumOdd'(x - 1)

-- Exercise 2
-- Write a function simpleLoop that computes the sum of the first n natural
-- numbers.
simpleLoop :: (Num a, Ord a) => a -> a -> a -> a
simpleLoop begval endval baseval
    | endval < begval = baseval
    | endval == begval = (+) begval baseval
    | otherwise = (+) endval (simpleLoop begval (endval - 1) baseval)

-- Exercise 3
-- Define a function simpleSum n that sums the natural numbers up to n 
-- in terms of the function simpleLoop.
simpleSum :: Int -> Int
simpleSum n = simpleLoop 0 n 0

-- Exercise 4
-- Write a function betterLoop by defining the function (+) as 
-- an extra arguement.
betterLoop :: (Num a, Ord a) => a -> a -> a -> (a -> a -> a) -> a
betterLoop begval endval baseval f
    | endval < begval = baseval
    | endval == begval = f begval baseval
    | otherwise = f endval (betterLoop begval (endval - 1) baseval f)

-- Exercise 5
-- Write the function fact and sum in terms of betterLoop.
bFact :: Int -> Int
bFact n = betterLoop 1 n 1 (*)

bSum :: Int -> Int
bSum n = betterLoop 0 n 0 (+)

-- Exercise 6
-- Generalise betterLoop further by implementing the function genLoop that
-- takes a fifth arguement that will handle computation on the value of the 
-- iterator.
genLoop :: (Num a, Ord a) => a -> a -> a -> (a -> a -> a) -> (a -> a) -> a
genLoop begval endval baseval f g
    | endval < begval = baseval
    | endval == begval = f (g begval) baseval
    | otherwise = f (g endval) (genLoop begval (endval - 1) baseval f g)

gGsum n = genLoop 0 n 0 (+) (2**)

gFact :: Int -> Int
gFact n = genLoop 1 n 1 (*) id

gSum :: Int -> Int
gSum n = genLoop 0 n 0 (+) id

-- Exercise 7
-- Write a function that doubles each element of a list and creates a
-- new list with the new values.
double :: Int -> Int
double x = 2 * x

map1 :: (Int -> Int) -> [Int] -> [Int]
map1 double [] = []
map1 double (x:xs) = double(x) : map1 double (xs)

-- Exercise 8
-- Write a function iterate that takes a natural number n, a function f 
-- ad an element x, and applies n times f on x.
iterate1 :: Int -> (Int -> Int) -> Int -> Int
iterate1 0 f x = x
iterate1 n f x = f (iterate1 (n-1) f x)

iterate2 :: Integer -> (a -> a) -> a -> a
iterate2 n f x
    | n < 0 = error "Negative arguement!"
    | n == 0 = x
    | otherwise = f (iterate2 (n-1) f x)