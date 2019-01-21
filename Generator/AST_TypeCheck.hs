{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures #-}

module AST_TypeCheck where

import AST
import Base
import Control.Monad (guard)

type TCEnv = [Type]

tcheckAdd :: TCEnv -> Fix LNG -> Fix LNG -> Maybe Type
tcheckAdd env x y = do
  TLit <- tcheck env x
  TLit <- tcheck env y
  return TLit

tcheckMul :: TCEnv -> Fix LNG -> Fix LNG -> Maybe Type
tcheckMul env x y = do
  TLit <- tcheck env x
  TLit <- tcheck env y
  return TLit

tcheckIf :: TCEnv -> Fix LNG -> Fix LNG -> Fix LNG -> Maybe Type
tcheckIf env x y z = do
  TBool <- tcheck env x
  t1 <- tcheck env y
  t2 <- tcheck env z
  guard $ t1 == t2
  return t1

tcheckEqual :: TCEnv -> Fix LNG -> Fix LNG -> Maybe Type
tcheckEqual env x y = do
  t1 <- tcheck env x
  t2 <- tcheck env y
  guard $ t1 == t2
  guard $ t1 == TBool || t1 == TLit
  return TBool

tcheckVar :: TCEnv -> Int -> Maybe Type
tcheckVar env x
  | x >= 0 && x < length env = Just $ env !! x
  | otherwise = Nothing

tcheckLam :: TCEnv -> Type -> Fix LNG -> Maybe Type
tcheckLam env t e = do
  t1 <- tcheck (t:env) e
  return $ TFunc t t1

tcheckApp :: TCEnv -> Fix LNG -> Fix LNG -> Maybe Type
tcheckApp env x y = do
  t1 <- tcheck env x
  t2 <- tcheck env y
  case t1 of
    TFunc t0 t -> if t0 == t2 then return t else Nothing
    _ -> Nothing

tcheck :: [Type] -> Fix LNG -> Maybe Type
tcheck env (LitP _)     = Just TLit
tcheck env (AddP x y)   = tcheckAdd env x y
tcheck env (MulP x y)   = tcheckMul env x y
tcheck env (BoolP _)    = Just TBool
tcheck env (IfP x y z)  = tcheckIf env x y z
tcheck env (EqualP x y) = tcheckEqual env x y
tcheck env (VarP x)     = tcheckVar env x
tcheck env (LamP t x)   = tcheckLam env t x
tcheck env (AppP x y)   = tcheckApp env x y
          
typeCheck :: Fix LNG -> Maybe Type
typeCheck = tcheck []
  