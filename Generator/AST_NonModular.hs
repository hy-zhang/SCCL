{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions, PatternSynonyms, ViewPatterns #-}

module AST_NonModular where

import Base

-- Datatypes

data Type = TLit | TBool | TFunc Type Type deriving (Eq, Show)

-- instance Show Type where
  -- show TBool = "Bool"
  -- show TLit = "Lit"
  -- show (TFunc t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

data Expr = Lit Int
          | Add Expr Expr
          | Mul Expr Expr
          | BoolV Bool
          | If Expr Expr Expr
          | Equal Expr Expr
          | Var Int
          | Lam Type Expr
          | App Expr Expr deriving Show


