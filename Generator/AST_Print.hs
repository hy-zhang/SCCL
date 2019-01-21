{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions, PatternSynonyms, ViewPatterns #-}

module AST_Print where

import Base
import AST

-- Pretty-printer

class PrettyAlg (f :: * -> *) where prettyAlg :: Alg f String
instance (PrettyAlg f, PrettyAlg g) => PrettyAlg (f :+: g) where
  prettyAlg (Inl x) = prettyAlg x
  prettyAlg (Inr x) = prettyAlg x

instance PrettyAlg LitF   where prettyAlg (Lit x)     = show x
instance PrettyAlg AddF   where prettyAlg (Add x y)   = "(" ++ x ++ " + " ++ y ++ ")"
instance PrettyAlg MulF   where prettyAlg (Mul x y)   = "(" ++ x ++ " * " ++ y ++ ")"
instance PrettyAlg BoolF  where prettyAlg (BoolV x)   = show x
instance PrettyAlg IfF    where prettyAlg (If x y z)  = "(if " ++ x ++ " then " ++ y ++ " else " ++ z ++ ")"
instance PrettyAlg EqualF where prettyAlg (Equal x y) = "(" ++ x ++ " == " ++ y ++ ")"
instance PrettyAlg VarF   where prettyAlg (Var x)     = "@" ++ show x
instance PrettyAlg LamF   where prettyAlg (Lam t x)   = "(\\" ++ show t ++ ". " ++ x ++ ")"
instance PrettyAlg AppF   where prettyAlg (App x y)   = "(" ++ x ++ " " ++ y ++ ")"

pretty :: Fix LNG -> String
pretty = fold prettyAlg

-- Ugly-printer

class UglyAlg (f :: * -> *) where uglyAlg :: Alg f String
instance (UglyAlg f, UglyAlg g) => UglyAlg (f :+: g) where
  uglyAlg (Inl x) = uglyAlg x
  uglyAlg (Inr x) = uglyAlg x

instance UglyAlg LitF   where uglyAlg (Lit x)     = "Lit[" ++ show x ++ "]"
instance UglyAlg AddF   where uglyAlg (Add x y)   = "Add[" ++ x ++ "," ++ y ++ "]"
instance UglyAlg MulF   where uglyAlg (Mul x y)   = "Mul[" ++ x ++ "," ++ y ++ "]"
instance UglyAlg BoolF  where uglyAlg (BoolV x)   = "Bool[" ++ show x ++ "]"
instance UglyAlg IfF    where uglyAlg (If x y z)  = "If[" ++ x ++ "," ++ y ++ "," ++ z ++ "]"
instance UglyAlg EqualF where uglyAlg (Equal x y) = "Equal[" ++ x ++ "," ++ y ++ "]"
instance UglyAlg VarF   where uglyAlg (Var x)     = "Var[" ++ show x ++ "]"
instance UglyAlg LamF   where uglyAlg (Lam t x)   = "Lam[" ++ show t ++ "," ++ x ++ "]"
instance UglyAlg AppF   where uglyAlg (App x y)   = "App[" ++ x ++ "," ++ y ++ "]"

ugly :: Fix LNG -> String
ugly = fold uglyAlg

-- Constructor-printer

class CtrPrintAlg (f :: * -> *) where ctrPrintAlg :: Alg f String
instance (CtrPrintAlg f, CtrPrintAlg g) => CtrPrintAlg (f :+: g) where
  ctrPrintAlg (Inl x) = ctrPrintAlg x
  ctrPrintAlg (Inr x) = ctrPrintAlg x

instance CtrPrintAlg LitF   where ctrPrintAlg (Lit x)     = "Lit"
instance CtrPrintAlg AddF   where ctrPrintAlg (Add x y)   = "Add[" ++ x ++ "," ++ y ++ "]"
instance CtrPrintAlg MulF   where ctrPrintAlg (Mul x y)   = "Mul[" ++ x ++ "," ++ y ++ "]"
instance CtrPrintAlg BoolF  where ctrPrintAlg (BoolV x)   = "Bool"
instance CtrPrintAlg IfF    where ctrPrintAlg (If x y z)  = "If[" ++ x ++ "," ++ y ++ "," ++ z ++ "]"
instance CtrPrintAlg EqualF where ctrPrintAlg (Equal x y) = "Equal[" ++ x ++ "," ++ y ++ "]"
instance CtrPrintAlg VarF   where ctrPrintAlg (Var x)     = "Var"
instance CtrPrintAlg LamF   where ctrPrintAlg (Lam t x)   = "Lam[" ++ x ++ "]"
instance CtrPrintAlg AppF   where ctrPrintAlg (App x y)   = "App[" ++ x ++ "," ++ y ++ "]"

ctrPrint :: Fix LNG -> String
ctrPrint = fold ctrPrintAlg

-- Show

instance Show (Fix LNG) where show = pretty