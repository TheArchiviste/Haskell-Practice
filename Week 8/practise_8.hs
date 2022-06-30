-- Exercise 1
-- Write a function elem' x l that checks whether an
-- element x exists in a list l.
elem' :: Eq a => a -> [a] -> Bool       -- This is also an in-build function in the Haskell's libraries.
elem' x [] = False
elem' x (y:ys) =
    if x == y then True else elem' x (ys)

-- Exercise 2
-- Write a function take' n l that returns the first n elements of a list l.
take' :: Int -> [a] -> [a]
take' 0 l = []
take' n [] = []
take' n (x:xs)
    | n < 0 = error "The first arguement of the function cannot be less than 0!"
    | otherwise = x : (take' (n-1) xs)

-- Exercise 3
-- Write a definition for a data type Tree a representing binary trees with data stored in 
-- the leaves. Use data constructors Leaf and Branch:

-- Using the newly created type define a tree with two branches. The left branch should contain
-- a tree composed by the single leaf with value 1 and the right branch should be composed by a 
-- tree with two leaves storing the values 2 and 3:
data Tree a = Leaf a | Branch (Tree a) (Tree a)

-- Write a definition for a function height t which computes the height of a tree t,
-- where the height of a leaf is 1:
height :: Tree a -> Int
height (Leaf _) = 1
height (Branch l r) = 1 + max (height l) (height r)
    where max x y = if x > y then x else y


