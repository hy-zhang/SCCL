{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Benchmark_Modular_AST where

import Base

-- AST.
data Type = TLit | TBool | TFunc Type Type

data LitF   e = Lit Int        deriving (Functor, Foldable, Traversable)
data AddF   e = Add e e        deriving (Functor, Foldable, Traversable)
data MulF   e = Mul e e        deriving (Functor, Foldable, Traversable)
data BoolF  e = BoolV Bool     deriving (Functor, Foldable, Traversable)
data IfF    e = If e e e       deriving (Functor, Foldable, Traversable)
data EqualF e = Equal e e      deriving (Functor, Foldable, Traversable)
data VarF   e = Var Int        deriving (Functor, Foldable, Traversable)
data LamF   e = Lam Type e     deriving (Functor, Foldable, Traversable)
data AppF   e = App e e        deriving (Functor, Foldable, Traversable)

type LNG = LitF :+: AddF :+: MulF :+: BoolF :+: IfF :+: EqualF :+: VarF :+: LamF :+: AppF

instance Arity LitF   where arity _ = 0
instance Arity AddF   where arity _ = 2
instance Arity MulF   where arity _ = 2
instance Arity BoolF  where arity _ = 0
instance Arity IfF    where arity _ = 3
instance Arity EqualF where arity _ = 2
instance Arity VarF   where arity _ = 0
instance Arity LamF   where arity _ = 1
instance Arity AppF   where arity _ = 2