-- Exercise 1
-- Compute the square of a number using a function square.
square :: Integer -> Integer
square x = x * x

-- Exercise 2
-- Define a constant function.
fortytwo :: Integer -> Integer
fortytwo x = 42

-- Exercise 3
-- Define a non-terminating function.
infinity = infinity + 1

-- Exercise 4
-- Define a type for each of the above functions.

-- Exercise 5
-- Define a conditional function.
min x y = if x <= y then x  else y

-- Exercise 6
-- Define a conditiona function using a guarded equation.
sign x
    | x < 0 = -1
    | x == 0 = 0
    | otherwise = 1 -- cover the case x > 0 = 1

sign1 x = if x < 0 then -1 else if x == 0 then 0 else 1

-- Exercise 7
-- Define the factorial function, recursively, using Haskell.
fact :: Integer -> Integer
fact n
    | n > 0 = n * fact (n - 1)
    | n == 0 = 1
    | otherwise = error "Negative arguement!"

fact1 :: Integer -> Integer
fact1 n = if n == 0 then 1 else n * fact (n - 1)    -- This function is not guarded for negative values!!!

-- Exercise 8
-- Define the factorial function using pattern maching on its arguement.
fact2 :: Integer -> Integer
fact2 0 = 1
fact2 n = n * fact2 (n - 1)

-- Exercise 9
-- Define a function using local definitions.
fun x = a + 1
    where a = x / 2

fun1 x =
    let a = x / 2 in a + 1

fun2 x = sqr (successor x)
    where sqr z = x * x; successor x = x + 1

magnitude a b = sqrt (asq + bsq)
    where asq = a * a; bsq = b * b

-- Exercise 10
-- Define a couple of functions then use function composition to combine them.
len :: [Int] -> Int
len [] = 0
len (x:xs) = 1 + len xs

double :: Int -> Int
double x = 2 * x

sample :: [Int] -> Int
sample = double . len 


