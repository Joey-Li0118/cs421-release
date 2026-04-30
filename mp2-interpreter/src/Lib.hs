module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = case H.lookup s env of 
    Just val -> val
    Nothing -> ExnVal "No match in env"


--- ### Arithmetic


eval (IntOpExp "/" e1 e2) env = 
    case eval e2 env of
        IntVal 0 -> ExnVal "Division by 0"
        _ -> liftIntOp (div) (eval e1 env) (eval e2 env)
    
    
    

eval (IntOpExp op e1 e2) env = 
    case H.lookup op intOps of 
        Just op -> liftIntOp (op) (eval e1 env) (eval e2 env)

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env =  
    case H.lookup op boolOps of 
        Just op -> liftBoolOp (op) (eval e1 env) (eval e2 env)

eval (CompOpExp op e1 e2) env = 
     case H.lookup op compOps of 
        Just op -> liftCompOp (op) (eval e1 env) (eval e2 env)

--- ### If Expressions

eval (IfExp e1 e2 e3) env = 
    case eval e1 env of
        BoolVal True -> eval e2 env
        BoolVal False -> eval e3 env
        _ -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env =
    case eval e1 env of
        CloVal params body newenv -> eval body (H.union (H.fromList (zip params (map (\arg -> eval arg env) args))) newenv) -- for each argument in the function zip it with the parameter variables e.g ("x", IntExp 5)
        _ -> ExnVal "Apply to non-closure"

--- ### Let Expressions

-- LetExp [(String,Exp)] Exp
eval (LetExp pairs body) env =
    let newenv = H.union (H.fromList (map (\(name, expr) -> (name, eval expr env)) pairs)) env
    in eval body newenv  
    -- have to look up a variable in the expression then evaluate it
--body can be an (IntOpExp "/" e1 e2)
-- have to evaluate body using the env in pairs, so evaluate pairs first, then eval body newenv



--- Statements
--- ----------
firstOfThree :: (a, b, c) -> a
firstOfThree (x, _, _) = x
secOfThree (_, x, _) = x
thirOfThree (_, _, x) = x

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements
--
exec (SetStmt var e) penv env = ("", penv, H.union (H.fromList [(var, eval e env)] ) env)

--- ### Sequencing

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (x:xs)) penv env = (firstOfThree(exec x penv env) ++ firstOfThree(exec (SeqStmt xs) penv' env'), penv'', env'')
    where 
    penv' = secOfThree (exec x penv env)
    env' = thirOfThree (exec x penv env)
    penv'' = secOfThree (exec (SeqStmt xs) penv' env')
    env'' = thirOfThree (exec (SeqStmt xs) penv' env')



--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = 
    case eval e1 env of
        BoolVal True -> exec s1 penv env
        BoolVal False -> exec s2 penv env
        _ -> ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", H.insert name p penv, env)

exec (CallStmt name args) penv env = 
    case (H.lookup name penv) of 
        Just (ProcedureStmt name procedureargs body) -> 
            -- for every argument we want to pair it up with the value that it holds
            -- Add to env and have it be a newenv which we can then use to execute the procedure
            -- takes all initialized vars in the procedure
            let newenv = H.union (H.fromList (zip procedureargs (map (\arg -> eval arg env) args))) env
            in exec body penv newenv  
        _ -> ("Procedure " ++ name ++ " undefined", penv, env)