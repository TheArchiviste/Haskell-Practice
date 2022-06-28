-- Exercise 1
-- Extend the evaluator of integers to also evaluate boolean expression and execute commands.

-- Labels (variable names) are just strings:
type Label = String

-- We give a type for operators:
data Op = Plus | Minus | Mult | Div
data BOp = Sup | Inf | Equ

-- The abstract syntax of expressions and commands is reflected in the
-- type of expressions and commands:
data IExpr = Nat Int | Deref Label | Oper IExpr Op IExpr
data BExpr = TF Bool | BOper IExpr BOp IExpr | Not BExpr | And BExpr BExpr
data Comm = Skip | Assign Label IExpr | Seq Comm Comm | IfThEl BExpr Comm Comm | While BExpr Comm
data Prog = C Comm | E IExpr | B BExpr

-- The abstract machine:
data Control = P Prog | O Op | CAssign | CIf | CWhile | CNot | CAnd | BO BOp
data Result = RP Prog | RL Label

type ControlStack = [Control]
type ResultStack = [Result]
type Memory = Label -> Maybe Int

-- The Memory is a function Label -> Int that maps names to values.
-- We use the Maybe type defined in the Prelude as:
-- data Maybe a = Nothing | Just a
-- This is useful to handle the case when we ask for the value of an undefined
-- variable: then we return Nothing instead of generating an error, thus
-- allowing the calling function to cleanly handle the problem.
type State = (ControlStack, ResultStack, Memory)

-- State is the type of the configurations of the abstract machine.
-- There are two different cases when we cannot
-- further reduce: either we have reached a value or there was an error.
-- This is handled with a new concrete type:
data ExtState = Err | End | ValI Int | ValB Bool | St State

-- transl is just a dummy function to translate operators from their
-- abstract representations to what they are really intended to do:
translI :: Op -> (Int -> Int -> Int)
translI Plus = (+)
translI Minus = (-)
translI Mult = (*)
translI Div = div

translB :: BOp -> (Int -> Int -> Bool)
translB Inf = (<)
translB Sup = (>)
translB Equ = (==)

-- extend is used to add a variable in the memory
extend :: (Label -> Maybe Int) -> (Label, Int) -> (Label -> Maybe Int)
extend f (l, n) x 
    | x == l    = Just n
    | otherwise = f x

-- step is the exact translation of the rules of the abstract machine.
step :: State -> ExtState
step (P (E (Nat n)):c, r, s) = St (c,RP (E (Nat n)):r,s)
step (P (B (TF b)):c, r, s) = St (c,RP (B (TF b)):r,s)
step (P (B (Not b)):c, r, s) = St (P (B b):CNot:c,r,s)
step (P (B (And b1 b2)):c, r, s) = St (P (B b1):P (B b2):CAnd:c,r,s)
step (CNot:c, RP (B (TF b)):r, s) = St (c, RP (B (TF (not b))):r, s)
step (CAnd:c, RP (B (TF b2)):RP (B (TF b1)):r, s) = St (c, RP (B (TF (b1 && b2))):r, s)
step (P (E (Oper e1 op e2)):c, r, s) = St (P (E e1):P (E e2):O op:c,r,s)
step (O op:c, RP (E (Nat n2)):RP (E (Nat n1)):r, s) =
    let n = (translI op) n1 n2 in St (c,RP (E (Nat n)):r,s)
step (P (B (BOper e1 bop e2)):c, r, s) = St (P (E e1):P (E e2):BO bop:c,r,s)
step (BO bop:c, RP (E (Nat n2)):RP (E (Nat n1)):r, s) =
    let b = (translB bop) n1 n2 in St (c,RP (B (TF b)):r,s)
step (P (E (Deref l)):c, r, s) = case (s l) of
    Nothing -> Err
    Just n -> St (c,RP (E (Nat n)):r,s)
step (P (C Skip):c, r, s) = St (c,r,s)
step (P (C (Assign l e)):c, r, s) = St (P (E e):CAssign:c,RL l:r,s)
step (CAssign:c, RP (E (Nat n)):RL l:r, s) =
    let s'= extend s (l,n) in St (c,r,s')
step (P (C (IfThEl b c1 c2)):c, r, s) = St (P (B b):CIf:c,RP (C c1):RP (C c2):r,s)
step (CIf:c, RP (B (TF True)):RP c1:RP c2:r, s) = St (P c1:c,r,s)
step (CIf:c, RP (B (TF False)):RP c1:RP c2:r, s) = St (P c2:c,r,s)
step (P (C (While b c0)):c, r, s) = St (P (B b):CWhile:c,RP (B b):RP (C c0):r,s)
step (CWhile:c, RP (B (TF True)):RP (B b):RP (C c0):r, s) = St (P (C c0):P (C (While b c0)):c, r, s)
step (CWhile:c, RP (B (TF False)):RP (B b):RP (C c0):r, s) = St (c,r,s)
step ([], [], s) = End
step ([], [RP (E (Nat n))], s) = ValI n
step ([], [RP (B (TF b))], s) = ValB b
step _ = Err

-- evalst is then the recursive function that
-- applies step until we reach a final state
data Value = None | VInt Int | VBool Bool

evalst :: State -> Value
evalst st = case step st of
    Err -> None
    End -> None
    ValI n -> VInt n
    ValB b -> VBool b
    St st' -> evalst st'

-- Finally, eval is the function that evaluates programs:
eval :: Prog -> Value
eval p = evalst ([P p], [], s) 
    where s l = Nothing

-- To test the abstract machine:
test1 = eval (E (Oper (Oper (Nat 2) Plus (Nat 3)) Minus (Nat 1)))
test2 = eval (E (Oper (Nat 5) Div (Oper (Nat 7) Div (Nat 3))))
test3 = eval (E (Oper (Nat 40) Div (Oper (Nat 6) Plus (Nat 4))))

test4 = eval (B (BOper (Nat 20) Equ (Nat 20)))
test5 = eval (B (Not (B (BOper (Nat 30) Sup (Nat 100)))))

-- Test with variables:
test6 = evalst E ([E (Oper (Oper (Deref "two") Plus (Deref "three")) Minus (Deref "one"))], [], s) where
    s "one" = Just 1
    s "two" = Just 2
    s "three" = Just 3
    s _ = Nothing
