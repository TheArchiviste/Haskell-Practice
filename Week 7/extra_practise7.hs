-- Exercise 1
-- Define a function quad that raises its argument to the 4th power.
quad :: Int -> Int
quad n = x * x
    where x = n * n

sqr :: Int -> Int
sqr n = n * n

quad1 :: Int -> Int
quad1 n = sqr(sqr n)

-- Exercise 2
-- Define a function double that doubles its arugment.
double :: Int -> Int
double n = 2 * n

-- Exercise 3
-- Define two functions min and max that compute the minimum and maximum
-- of two numbers.
min :: Int -> Int -> Int
min x y
    | x < y = x
    | y < x = y 
    | otherwise = error "The numbers are equal!"

max :: Int -> Int -> Int
max x y
    | x > y = x
    | y > x = y
    | otherwise = error "The numbers are equal!"

-- Exercise 4
-- Define a function fact that takes as arguement a natural number and gives
-- the computation of n!.
fact :: Int -> Int
fact n
    | n == 0 = 1
    | n > 0 = n * fact(n - 1)
    | otherwise = error "The number is negative!"

-- Exercise 5
-- Define function mult that multiplies two integer numbers and a function
-- infinity that that is a non-terminating function.
mult :: Integer -> Integer -> Integer
mult x y
    | x == 0 = 0
    | y == 0 = 0
    | otherwise = x * y

infinity = infinity + 1

-- Exercise 6
-- Define a Haskell function absVal that computes the absolute value of an integer.
absVal :: Int -> Int
absVal x
    | x >= 0 = x
    | otherwise = -x

-- Exercise 7
-- Define a recusive function fib to compute the Fibonnaci numbers.
fib n 
    | n == 0 = 1 
    | n == 1 = 1
    | n > 1 = fib(n - 1) + fib(n - 2)
    | otherwise = error "Negative arguement!"

-- Exercise 8
-- Define a Haskell function induction base comb arg to implement a general inductive
-- definition of some other function, where base is the value to be returned for the base case of the
-- inductive definition of the function; comb is a binary function to apply to an argument greater
-- than the value of the base case; and arg is the value to be used in the function’s recursive call.
