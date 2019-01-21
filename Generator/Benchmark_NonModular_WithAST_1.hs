{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Benchmark_NonModular_WithAST_1 where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad

data Expr = Lit Int
          | Add Expr Expr
          | Mul Expr Expr deriving Show

instance Arbitrary Expr where
  arbitrary = getSize >>= gen
    where gen :: Int -> Gen Expr
          gen n | n <= 0 = genLit n
                | otherwise = frequency [(1, genAdd n), (1, genMul n)]
          genLit :: Int -> Gen Expr
          genLit n = liftM Lit $ choose (0, 100)
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
