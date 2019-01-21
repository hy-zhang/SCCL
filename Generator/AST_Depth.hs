{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module AST_Depth where

import Base
import AST

class DepthAlg (f :: * -> *) where depthAlg :: Alg f Int
instance (DepthAlg f, DepthAlg g) => DepthAlg (f :+: g) where
  depthAlg (Inl x) = depthAlg x
  depthAlg (Inr x) = depthAlg x

instance DepthAlg LitF   where depthAlg  _          = 0
instance DepthAlg AddF   where depthAlg (Add x y)   = 1 + max x y
instance DepthAlg MulF   where depthAlg (Mul x y)   = 1 + max x y
instance DepthAlg BoolF  where depthAlg  _          = 0
instance DepthAlg IfF    where depthAlg (If x y z)  = 1 + max x (max y z)
instance DepthAlg EqualF where depthAlg (Equal x y) = 1 + max x y
instance DepthAlg VarF   where depthAlg  _          = 0
instance DepthAlg LamF   where depthAlg (Lam _ x)   = 1 + x
instance DepthAlg AppF   where depthAlg (App x y)   = 1 + max x y

class SizeAlg (f :: * -> *) where sizeAlg :: Alg f Int
instance (SizeAlg f, SizeAlg g) => SizeAlg (f :+: g) where
  sizeAlg (Inl x) = sizeAlg x
  sizeAlg (Inr x) = sizeAlg x

instance SizeAlg LitF   where sizeAlg  _          = 1
instance SizeAlg AddF   where sizeAlg (Add x y)   = 1 + x + y
instance SizeAlg MulF   where sizeAlg (Mul x y)   = 1 + x + y
instance SizeAlg BoolF  where sizeAlg  _          = 1
instance SizeAlg IfF    where sizeAlg (If x y z)  = 1 + x + y + z
instance SizeAlg EqualF where sizeAlg (Equal x y) = 1 + x + y
instance SizeAlg VarF   where sizeAlg  _          = 1
instance SizeAlg LamF   where sizeAlg (Lam _ x)   = 1 + x
instance SizeAlg AppF   where sizeAlg (App x y)   = 1 + x + y

getDepth :: Fix LNG -> Int
getDepth = fold depthAlg

getSize :: Fix LNG -> Int
getSize = fold sizeAlg