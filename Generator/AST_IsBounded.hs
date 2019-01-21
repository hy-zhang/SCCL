{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions, PatternSynonyms, ViewPatterns #-}

module AST_IsBounded where

import Base
import AST

class BoundAlg (f :: * -> *) where boundAlg :: Alg f (Int -> Bool)
instance (BoundAlg f, BoundAlg g) => BoundAlg (f :+: g) where
  boundAlg (Inl x) = boundAlg x
  boundAlg (Inr x) = boundAlg x

instance BoundAlg LitF   where boundAlg _ _ = True
instance BoundAlg AddF   where boundAlg (Add x y) n = x n && y n
instance BoundAlg MulF   where boundAlg (Mul x y) n = x n && y n
instance BoundAlg BoolF  where boundAlg _ _ = True
instance BoundAlg IfF    where boundAlg (If x y z) n = x n && y n && z n
instance BoundAlg EqualF where boundAlg (Equal x y) n = x n && y n
instance BoundAlg VarF   where boundAlg (Var x) n = x <= n
instance BoundAlg LamF   where boundAlg (Lam _ x) n = x (n + 1)
instance BoundAlg AppF   where boundAlg (App x y) n = x n && y n

isBounded :: Fix LNG -> Bool
isBounded e = fold boundAlg e (-1)