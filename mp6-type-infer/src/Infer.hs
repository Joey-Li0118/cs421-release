module Infer where

import Common

import Control.Monad.Writer (listen)
import Control.Monad.Except (throwError)
import Data.Map.Strict as H (Map, insert, lookup, empty, fromList, singleton)

tag1 = 64002
tag2 = 68869
tag3 = 19545

  {- question 1: fresh instance function -}

freshInst :: PolyTy -> Infer MonoTy
freshInst (Forall qVars tau) = 
  do 
--     freshTau :: Infer MonoTy
-- freshTau = TyVar <$> fresh
-- data PolyTy = Forall [VarId] MonoTy
--for each qvar in array, get a freshvariable
    freshVars <- mapM (\x -> freshTau) qVars 
    let subst = H.fromList (zip qVars freshVars)
    return ( apply subst tau)
  

  {- question 2: occurs check -}

occurs :: VarId -> MonoTy -> Bool
occurs i tau = case tau of 
  (TyVar varid) -> (varid == i) 
  (TyConst s []) -> False 
  (TyConst s (x:xs)) -> occurs i x || occurs i (TyConst s xs)


  {- question 3: unification -}

unify :: [Constraint] -> Infer Substitution
unify [] = return substEmpty
unify (x:xs) = case x of 
  (t1 :~: t2 ) -> case (t1, t2) of 
    (TyVar s, TyVar t)
      | s == t -> unify xs
      | otherwise -> do
        let sub = substInit s (TyVar t) 
        let phi = map (apply sub) xs 
        sigma <- unify phi 
        return (substCompose sigma sub)
      | occurs s t2 -> throwError (InfiniteType s t2)  -- eliminate rule substitute all occurences of s in φ′with t to get φ′′. Let σ be the substitution resulting from unifying φ′′. Return σ updated with s 7→σ(t)
      -- | otherwise -> 
    (s, TyVar tau) -> unify ((TyVar tau :~: s):xs)
    -- (tau,TyVar v2)->
    -- (TyConst c1 v1, TyConst c2 v2) -> 

  {- question 4: type inference -}

infer :: TypeEnv -> Exp -> Infer MonoTy
infer env exp = undefined

inferInit :: TypeEnv -> Exp -> Infer PolyTy
inferInit env e = do
  (tau, constraints) <- listen $ infer env e
  substitution <- unify constraints
  return $ quantifyMonoTy $ apply substitution tau

inferDec :: TypeEnv -> Dec -> Infer (TypeEnv, PolyTy)
inferDec env (AnonDec e') = do
  tau <- inferInit env e'
  return (env, tau)
inferDec env (LetDec x e') = do
  tau <- inferInit env (LetExp x e' (VarExp x))
  return (H.insert x tau env, tau)
inferDec env (LetRec f x e') = do
  tau <- inferInit env (LetRecExp f x e' (VarExp f))
  return (H.insert f tau env, tau)
