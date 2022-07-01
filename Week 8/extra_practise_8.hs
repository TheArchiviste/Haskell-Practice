-- Exercise 1
-- Wriite a function roots that finds the roots of quadratic equation.
sqr :: Double -> Double
sqr x = x * x

roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c)
    | a == 0 = error "Not a quadratic equation!"
    | d < 0 = error "Complex roots!"
    | otherwise = (((-b) - r)/e, ((-b) + r)/e)
        where {
            d = sqr b - 4 * a * c;
            r = sqrt d;
            e = 2 * a;
        }

-- Exercise 2
-- Write a function cartProd to comput the cartesian product of two finite sets given as lists.

-- 1st solution:
cardProd :: [a] -> [b] -> [(a,b)]
cardProd xs ys = [(x, y) | x <- xs, y <- ys]

-- 2nd solution:
combine :: a -> [b] -> [(a,b)]
combine _ [] = []
combine x (y:ys) = [(x,y)] ++ combine x ys

cardProd' :: [a] -> [b] -> [(a,b)]
cardProd' [] ys = []
cardProd' (x:xs) ys = combine x ys ++ cardProd' xs ys