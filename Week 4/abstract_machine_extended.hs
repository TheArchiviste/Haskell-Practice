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
type ResultStack = [Int]
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
eval p = eva;st ([P p], [], s)
    where s l = Nothing
