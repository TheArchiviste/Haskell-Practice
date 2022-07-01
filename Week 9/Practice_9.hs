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
