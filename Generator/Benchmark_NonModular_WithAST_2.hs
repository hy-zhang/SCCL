{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Benchmark_NonModular_WithAST_2 where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad

data Expr = Lit Int
          | Add Expr Expr
          | Mul Expr Expr
          | BoolV Bool
          | If Expr Expr Expr
          | Equal Expr Expr deriving Show

instance Arbitrary Expr where
  arbitrary = getSize >>= gen
    where gen :: Int -> Gen Expr
          gen n | n <= 0 = frequency [(1, genLit n), (1, genBool n)]
                | otherwise = frequency [(1, genAdd n), (1, genMul n), (1, genIf n), (1, genEqual n)]
          genLit :: Int -> Gen Expr
          genLit n = liftM Lit $ choose (0, 100)
          genBool :: Int -> Gen Expr
          genBool n = liftM BoolV $ choose (False, True)
          genAdd :: Int -> Gen Expr
          genAdd n = do n' <- choose (0, n - 1)
                        eA <- gen n'
                        eB <- gen (n - 1 - n')
                        return $ Add eA eB
          genMul :: Int -> Gen Expr
          genMul n = do n' <- choose (0, n - 1)
                        eA <- gen n'
                        eB <- gen (n - 1 - n')
                        return $ Mul eA eB
          genIf :: Int -> Gen Expr
          genIf  n = do nA <- choose (0, n - 1)
                        nB <- choose (0, n - 1 - nA)
                        eA <- gen nA
                        eB <- gen nB
                        eC <- gen (n - 1 - nA - nB)
                        return $ If eA eB eC
          genEqual :: Int -> Gen Expr
          genEqual n = do n' <- choose (0, n - 1)
                          eA <- gen n'
                          eB <- gen (n - 1 - n')
                          return $ Equal eA eB
