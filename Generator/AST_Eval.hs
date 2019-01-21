{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures #-}

module AST_Eval where

import Base
import AST
import AST_TypeCheck

projLit :: LitF :<: f => Fix f -> Maybe Int
projLit (In e) = case proj e of
  Just (Lit x) -> Just x
  _            -> Nothing

projBool :: BoolF :<: f => Fix f -> Maybe Bool
projBool (In e) = case proj e of
  Just (BoolV x) -> Just x
  _              -> Nothing

projLam :: LamF :<: f => Fix f -> Maybe (Type, Fix f)
projLam (In e) = case proj e of
  Just (Lam t e') -> Just (t, e')
  _               -> Nothing

evalAdd :: Fix LNG -> Fix LNG -> Maybe (Fix LNG)
evalAdd (LitP x) (LitP y) = return . lit $ x + y
evalAdd e1       e2       = case (eval e1, eval e2) of
    (Just e1', _) -> return $ add e1' e2
    (_, Just e2') -> return $ add e1 e2'
    _             -> Nothing

evalMul :: Fix LNG -> Fix LNG -> Maybe (Fix LNG)
evalMul (LitP x) (LitP y) = return . lit $ x * y
evalMul e1       e2       = case (eval e1, eval e2) of
    (Just e1', _) -> return $ mul e1' e2
    (_, Just e2') -> return $ mul e1 e2'
    _             -> Nothing

evalIf :: Fix LNG -> Fix LNG -> Fix LNG -> Maybe (Fix LNG)
evalIf (BoolP True)  e  _  = Just e
evalIf (BoolP False) _  e  = Just e
evalIf e1            e2 e3 = case (eval e1, eval e2, eval e3) of
    (Just e1', _, _) -> return $ ifC e1' e2 e3
    (_, Just e2', _) -> return $ ifC e1 e2' e3
    (_, _, Just e3') -> return $ ifC e1 e2 e3'
    _                -> Nothing

evalEqual :: Fix LNG -> Fix LNG -> Maybe (Fix LNG)
evalEqual (LitP x)  (LitP y)  = return . boolV $ x == y
evalEqual (BoolP x) (BoolP y) = return . boolV $ x == y
evalEqual e1        e2        = case (eval e1, eval e2) of
    (Just e1', _) -> return $ equal e1' e2
    (_, Just e2') -> return $ equal e1 e2'
    _             -> Nothing

evalLam :: Type -> Fix LNG -> Maybe (Fix LNG)
evalLam t e = case eval e of
    Just e' -> return $ lam t e'
    _       -> Nothing

evalApp :: Fix LNG -> Fix LNG -> Maybe (Fix LNG)
evalApp (LamP _ e) e2 = return . shift (-1) 0 $ subst 0 (shift 1 0 e2) e
evalApp e1         e2 = case (eval e1, eval e2) of
    (Just e1', _) -> return $ app e1' e2
    (_, Just e2') -> return $ app e1 e2'
    _             -> Nothing

eval :: Fix LNG -> Maybe (Fix LNG)
eval (LitP _)       = Nothing
eval (AddP x y)     = evalAdd x y
eval (MulP x y)     = evalMul x y
eval (BoolP _)      = Nothing
eval (IfP x y z)    = evalIf x y z
eval (EqualP x y)   = evalEqual x y
eval (VarP _)       = Nothing
eval (LamP t x)     = evalLam t x
eval (AppP x y)     = evalApp x y

subst :: Int -> Fix LNG -> Fix LNG -> Fix LNG
subst n x (VarP v)   = if v == n then x else var v
subst n x (LamP t e) = lam t $ subst (n + 1) (shift 1 0 x) e
subst n x (In e)     = In . fmap (subst n x) $ e

shift :: Int -> Int -> Fix LNG -> Fix LNG
shift i c (VarP n)   = if n < c then var n else var (n + i)
shift i c (LamP t e) = lam t $ shift i (c + 1) e
shift i c (In e)     = In . fmap (shift i c) $ e

evaluate :: Fix LNG -> Fix LNG
evaluate e = case eval e of
    Just e' -> evaluate e'
    _       -> e
