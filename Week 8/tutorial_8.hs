-- Exercise 1
check = "cat" == 'c' : ['a', 't']

-- Exercise 2
-- Define a function mod' that computes the modulus of a 
-- number divided by another.
mod' :: Int -> Int -> Int
mod' x y
    | x - y < 0 = x
    | otherwise = mod' (x - y) y

-- Exercise 2
-- Define a function even' that checks whether a number is 
-- indeed even.
even' :: Int -> Bool
even' x
    | mod' x 2 == 0 = True
    | otherwise = False

-- Exercise 3
-- Define a function that computes the collatz conjecture. 
collatz x
    | x == 1 = 1
    | even' x = div x 2
    | otherwise = 3 * (x + 1)