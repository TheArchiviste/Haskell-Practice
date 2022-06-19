import Prelude hiding (map)

-- Exercise 1
-- Function that takes a number n as arguement and gives n^4 as  a result. 

power4 n = n * n * n * n

toThePower4 n = square (square n)
    where square n = n * n

sqrOfSqr x = y * y
    where y = x * x

-- Exercise 2 
-- Function that takes a list as argument and gives its length as a result.

lenOfList [] = 0
lenOfList (x:xs) = 1 + lenOfList xs    -- Recursive call.

-- Exercise 3
-- Function map that takes as arguments a function and a  list, and returns the list
-- of the reuslts of the application of that function to each element of the list.

map f [] = []
map f (x:xs) = f x : map f xs