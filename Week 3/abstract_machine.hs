-- Exercise 1
-- Write a program using your favourite programming language to evaluate integer expressions in
-- SIMP. Assume the input of your program is a syntax tree corresponding to an integer expression,
-- and execute it using the transition rules of the abstract machine:


-- Labels (variable names) are just strings:
type Label = String

-- We give an abstract representation of operators and expressions:
data Op = Plus | Minus | Mult | Div
data Expr = Nat Int | Deref Label | Oper Expr Op Expr

-- An element of the control stack of the abstract machine may be
-- either an expression or an operator, therefore we define a new type:
data Control = E Expr | O Op

type ControlStack = [Control]
type ResultStack = [Int]
type Memory = Label -> Maybe Int

-- The Memory is a function Label -> Int that maps names to values.
-- We use the Maybe type defined in the Prelude as:
-- data Maybe a = Nothing | Just a
-- This is useful to handle the case when we ask for the value of an undefined
-- variable: then we return Nothing instead of generating an error.
type State = (ControlStack, ResultStack, Memory)

-- State is the type of the configurations of the abstract machine.
-- There are two different cases when we cannot
-- further reduce: either we have reached a value or there was an error.
-- This is handled with a new concrete type:
data ExtState = Err | Val Int | St State

-- transl is just a dummy function to translate operators from their
-- abstract representations to what they are really intended to do:
transl :: Op -> (Int -> Int -> Int)
transl Plus = (+)
transl Minus = (-)
transl Mult = (*)
transl Div = div

-- step is the exact translation of the rules of the abstract machine.
step :: State -> ExtState
step (E (Nat n):c, r, s) = St (c, n:r, s)
step (E (Oper e1 op e2):c, r, s) = St (E e1: E e2: O op:c, r, s)
step (O op:c, n2:n1:r, s) = let n = (transl op) n1 n2 in St (c, n:r, s)
step (E (Deref l):c, r, s) = case (s l) of
    Nothing -> Err
    Just n -> St (c, n:r, s)
step ([], [n], s) = Val n
step _ = Err

-- evalst applies step until we reach a final state, that
-- is either a value or an error.
evalst :: State -> Maybe Int
evalst st = case step st of
    Err -> Nothing
    Val n -> Just n
    St st' -> evalst st'

-- Finally, eval is the function that evaluates arithmetic expressions:
eval :: Expr -> Int
eval e = case evalst([E e], [], s) of
    Just n -> n
    Nothing -> error "Evaluation failed!"
    where s l = Nothing

-- To test the abstract machine:
test1 = eval (Oper (Oper (Nat 2) Plus (Nat 3)) Minus (Nat 1))
test2 = eval (Oper (Nat 5) Div (Oper (Nat 7) Div (Nat 3)))
test3 = eval (Oper (Nat 40) Div (Oper (Nat 6) Plus (Nat 4)))

-- Test with variables:
test4 = evalst ([E (Oper (Oper (Deref "two") Plus (Deref "three")) Minus (Deref "one"))], [], s) where
    s "one" = Just 1
    s "two" = Just 2
    s "three" = Just 3
    s _ = Nothing
