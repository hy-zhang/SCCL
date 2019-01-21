{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Benchmark_NonModular1 where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Benchmark_NonModular_AST

-- Size = #internal constructors.
type Size = Int
type MaxId = Int
type Seed = (Size, MaxId)

-- Generator.
instance Arbitrary Expr where
  arbitrary = getSize >>= \n -> gen (n, -1)
    where gen :: Seed -> Gen Expr
          gen s@(n, max) | n <= 0 && max >= 0 = frequency [(1, genLit s), (1, genBool s), (1, genVar s)]
                         | n <= 0 = frequency [(1, genLit s), (1, genBool s)]
                         | otherwise = frequency [(1, genAdd s), (1, genMul s), (1, genIf s),
                                                  (1, genEqual s), (1, genLam s), (1, genApp s)]
          genLit :: Seed -> Gen Expr
          genLit (n, max) = liftM Lit $ choose (0, 100)
          genBool :: Seed -> Gen Expr
          genBool (n, max) = liftM BoolV $ choose (False, True)
          genVar :: Seed -> Gen Expr
          genVar (n, max) = liftM Var $ choose (0, max)
          genAdd :: Seed -> Gen Expr
          genAdd (n, max) = do n' <- choose (0, n - 1)
                               eA <- gen (n', max)
                               eB <- gen (n - 1 - n', max)
                               return $ Add eA eB
          genMul :: Seed -> Gen Expr
          genMul   (n, max) = do n' <- choose (0, n - 1)
                                 eA <- gen (n', max)
                                 eB <- gen (n - 1 - n', max)
                                 return $ Mul eA eB
          genIf :: Seed -> Gen Expr
          genIf    (n, max) = do nA <- choose (0, n - 1)
                                 nB <- choose (0, n - 1 - nA)
                                 eA <- gen (nA, max)
                                 eB <- gen (nB, max)
                                 eC <- gen (n - 1 - nA - nB, max)
                                 return $ If eA eB eC
          genEqual :: Seed -> Gen Expr
          genEqual (n, max) = do n' <- choose (0, n - 1)
                                 eA <- gen (n', max)
                                 eB <- gen (n - 1 - n', max)
                                 return $ Equal eA eB
          genLam :: Seed -> Gen Expr
          genLam   (n, max) = do t1 <- elements [TBool, TLit]
                                 t2 <- elements [TBool, TLit]
                                 t  <- elements [TBool, TLit, TFunc t1 t2]
                                 e  <- gen (n - 1, max + 1)
                                 return $ Lam t e
          genApp :: Seed -> Gen Expr
          genApp   (n, max) = do n' <- choose (0, n - 1)
                                 eA <- gen (n', max)
                                 eB <- gen (n - 1 - n', max)
                                 return $ App eA eB
