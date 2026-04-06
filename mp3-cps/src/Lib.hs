--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 funct = funct 1
factk integer funct = factk (integer-1) (\x -> funct(x * integer))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] even odd
    | x `mod` 2 == 0 = even x
    | otherwise = odd x

evenoddk (x:xs) even odd 
    | x `mod` 2 == 0 = evenoddk xs (\result -> even(result + x)) odd
    | otherwise = evenoddk xs even (\result -> odd(result + x))
    

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple expression = 
    case expression of 
        (AppExp _ _) -> False
        (OpExp op a b) -> isSimple a && isSimple b
        (IfExp a b c) -> isSimple a && isSimple b && isSimple c
        _ -> True

--- ### Define `cpsExp` - Overview



-- data Exp = IntExp Integer
--          | VarExp String
--          | LamExp String Exp
--          | IfExp Exp Exp Exp
--          | OpExp String Exp Exp
--          | AppExp Exp Exp
--          deriving (Eq)

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
cpsExp a k int = 
    case a of 
        VarExp _ -> (AppExp k a, int)
        IntExp _ -> (AppExp k a, int)
        AppExp f e 
            | isSimple e == True -> (AppExp (AppExp f e) k, int) 
            | otherwise -> cpsExp e (LamExp v (AppExp (AppExp f (VarExp v)) k)) int1
                where (v, int1) = gensym int
        
        OpExp op e1 e2 
            | isSimple e1 && isSimple e2 -> (AppExp k (OpExp op e1 e2), int)
            | isSimple e1 -> cpsExp e2 ( LamExp v (AppExp k (OpExp op e1 (VarExp v) ))) int1
            | isSimple e2 -> cpsExp e1 ( LamExp v (AppExp k (OpExp op (VarExp v) e2 ))) int1
            | otherwise -> 
                let 
                    (v, int1)  = gensym int
                    (v1, int2) = gensym int1
                in 
                    cpsExp e1 (LamExp v (fst(cpsExp e2 (LamExp v1 (AppExp k (OpExp op (VarExp v) (VarExp v1)))) int2))) int1
            where (v, int1)  = gensym int

        IfExp e1 e2 e3 
            | isSimple e1 -> 
                let 
                    (true, int1) = cpsExp e2 k int
                    (false, int2) = cpsExp e3 k int1
                in
                    (IfExp e1 true false, int2)
            | otherwise ->
                let 
                    (v, int1) = gensym int
                in cpsExp e1 (LamExp v (IfExp (VarExp v) (fst(cpsExp e2 k int)) (fst(cpsExp e3 k int)))) int1
--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl string params exp) = 
    let 
        k = "k"
        (newexp, _) = cpsExp exp (VarExp k) 0
    in
        Decl string (params ++ [k]) newexp 
