-- Exercise 1
-- Test on what the outcome of the expressions will be and their respective types using Haskell.
strConc = "CS2" ++ "PLD"

boolEval = True || False

mult = 42 * 42

sqrtOf = sqrt 2

-- Exercise 2
-- Write a function that computes the factorial sequence for any integer number x.
fact x = if x == 0 then 1 else x * fact (x-1)