{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Benchmark_Modular_WithAST where

import Control.Monad hiding (when)
import Benchmark_Modular_AST
import Base
import Base_MonadTrans
import Base_RandMonad
import Base_GenCombinators

type Size = Int
type MaxId = Int
type Seed = (Size, MaxId)

instance Derive Seed Int where derive = fst

when :: MonadMaybe m => Bool -> m a -> m a
when True x = x
when _ _    = none

liftS :: (Functor f, Functor m) => CoAlgM m f Size -> CoAlgM m f Seed
liftS c (size, max) = fmap (fmap (\x -> (x, max))) (c size)

gLit :: (MonadMaybe m, MonadRand m) => CoAlgM m LitF Seed
gLit = liftS $ \n -> when (n <= 0) (liftM Lit $ choose (0, 100))

gAdd :: (MonadMaybe m, MonadRand m) => CoAlgM m AddF Seed
gAdd = liftS $ \n -> when (n > 0) $ do
  n' <- choose (0, n - 1)
  elements [Add n' (n - 1), Add (n - 1) n']

gMul :: (MonadMaybe m, MonadRand m) => CoAlgM m MulF Seed
gMul = liftS $ \n -> when (n > 0) $ do
  n' <- choose (0, n - 1)
  elements [Mul n' (n - 1), Mul (n - 1) n']

gBool :: (MonadMaybe m, MonadRand m) => CoAlgM m BoolF Seed
gBool = liftS $ \n -> when (n <= 0) (liftM BoolV $ choose (False, True))

gIf :: (MonadMaybe m, MonadRand m) => CoAlgM m IfF Seed
gIf = liftS $ \n -> when (n > 0) $ do
  nA <- choose (0, n - 1)
  nB <- choose (0, n - 1)
  elements [If (n - 1) nA nB , If nA (n - 1) nB, If nA nB (n - 1)]

gEqual :: (MonadMaybe m, MonadRand m) => CoAlgM m EqualF Seed
gEqual = liftS $ \n -> when (n > 0) $ do
  n' <- choose (0, n - 1)
  elements [Equal n' (n - 1), Equal (n - 1) n']

gVar :: (MonadMaybe m, MonadRand m) => CoAlgM m VarF Seed
gVar (n, max) = when (n <= 0 && max >= 0) (liftM Var $ choose (0, max))

gLam :: (MonadMaybe m, MonadRand m) => CoAlgM m LamF Seed
gLam (n, max) = when (n > 0) $ do
  t1 <- elements [TBool, TLit]
  t2 <- elements [TBool, TLit]
  t  <- elements [TBool, TLit, TFunc t1 t2]
  return $ Lam t (n - 1, max + 1)

gApp :: (MonadMaybe m, MonadRand m) => CoAlgM m AppF Seed
gApp = liftS $ \n -> when (n > 0) $ do
  n' <- choose (0, n - 1)
  elements [App n' (n - 1), App (n - 1) n']

runArith :: Size -> IO (Fix (LitF :+: AddF :+: MulF))
runArith n = generateW (replicate 3 1) (n, -1)
  (gLit |**| gAdd |**| gMul)

runArithBool :: Size -> IO (Fix (LitF :+: AddF :+: MulF :+: BoolF +: IfF :+: EqualF))
runArithBool n = generateW (replicate 6 1) (n, -1)
  (gLit |**| gAdd |**| gMul |**| gBool |**| gIf |**| gEqual)

runArithBoolLam :: Size -> IO (Fix (LitF :+: AddF :+: MulF :+: BoolF +: IfF :+: EqualF :+: VarF :+: LamF + AppF))
runArithBoolLam n = generateW (replicate 9 1) (n, -1)
  (gLit |**| gAdd |**| gMul |**| gBool |**| gIf |**| gEqual |**| gVar |**| gLam |**| gApp)

