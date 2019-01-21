{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Benchmark_NonModular3 where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Benchmark_NonModular_AST

-- Size = #internal constructors.
type Size = Int
type MaxId = Int
type Seed = (Size, MaxId)

instance Arbitrary MaybeExpr where
  arbitrary = getSize >>= \n -> mToMaybe 10000 n $ gen (n, -1)
    where gen :: Seed -> M Expr
          gen s@(n, max) = do
            c <- getCounter
            if c <= 0 then error "Size bound exceeded."
                      else if n <= 0 && max >= 0 then select s [wLit, wBool, wVar]
                                                 else if n <= 0 then select s [wLit, wBool]
                                                                else select s [wAdd, wMul, wIf, wEqual, wLam, wApp]
          select :: Seed -> [(Int, Seed -> M Expr)] -> M Expr
          select s@(n,_) gens = do
            maxD <- getMaxDepth
            let weights = map (\x -> getWeight (fst x) 3 n maxD) gens
            rs <- permutation weights 
            update >> (foldl1 (\(M x) (M y) -> M (x `mplus` y)) $ fmap (\i -> snd (gens !! i) s) rs)
          wLit   = (0, genLit)
          wAdd   = (2, genAdd)
          wMul   = (2, genMul)
          wBool  = (0, genBool)
          wIf    = (3, genIf)
          wEqual = (2, genEqual)
          wVar   = (0, genVar)
          wLam   = (1, genLam)
          wApp   = (2, genApp)
          genLit :: Seed -> M Expr
          genLit (n, max) = liftM Lit $ mChoose (0, 100)
          genBool :: Seed -> M Expr
          genBool (n, max) = liftM BoolV $ mChoose (False, True)
          genVar :: Seed -> M Expr
          genVar (n, max) = liftM Var $ mChoose (0, max)
          genAdd :: Seed -> M Expr
          genAdd (n, max) = do n' <- mChoose (0, n - 1)
                               eA <- gen (n', max)
                               eB <- gen (n - 1 - n', max)
                               return $ Add eA eB
          genMul :: Seed -> M Expr
          genMul   (n, max) = do n' <- mChoose (0, n - 1)
                                 eA <- gen (n', max)
                                 eB <- gen (n - 1 - n', max)
                                 return $ Mul eA eB
          genIf :: Seed -> M Expr
          genIf    (n, max) = do nA <- mChoose (0, n - 1)
                                 nB <- mChoose (0, n - 1 - nA)
                                 eA <- gen (nA, max)
                                 eB <- gen (nB, max)
                                 eC <- gen (n - 1 - nA - nB, max)
                                 return $ If eA eB eC
          genEqual :: Seed -> M Expr
          genEqual (n, max) = do n' <- mChoose (0, n - 1)
                                 eA <- gen (n', max)
                                 eB <- gen (n - 1 - n', max)
                                 return $ Equal eA eB
          genLam :: Seed -> M Expr
          genLam   (n, max) = do t1 <- mElements [TBool, TLit]
                                 t2 <- mElements [TBool, TLit]
                                 t  <- mElements [TBool, TLit, TFunc t1 t2]
                                 e  <- gen (n - 1, max + 1)
                                 return $ Lam t e
          genApp :: Seed -> M Expr
          genApp   (n, max) = do n' <- mChoose (0, n - 1)
                                 eA <- gen (n', max)
                                 eB <- gen (n - 1 - n', max)
                                 return $ App eA eB
