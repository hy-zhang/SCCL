{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Benchmark_NonModular_AST where

import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Test.QuickCheck.Gen
import System.Random
import Prelude hiding (iterate)
import Base (prob)

-- AST.
data Expr = Lit Int
          | Add Expr Expr
          | Mul Expr Expr
          | BoolV Bool
          | If Expr Expr Expr
          | Equal Expr Expr
          | Var Int
          | Lam Type Expr
          | App Expr Expr deriving Show

data Type = TLit | TBool | TFunc Type Type deriving Show

arity :: Type -> Int
arity (TFunc t1 t2) = 1 + arity t2
arity  _            = 0

data MaybeExpr = MaybeExpr { unMaybe :: Maybe Expr }

data M a = M { unM :: MaybeT (StateT Int (ReaderT Int Gen)) a } deriving Functor

instance Monad M where
  return = M . return
  (M m) >>= f = M $ m >>= unM . f

instance Applicative M where
  pure = M . pure
  (M f) <*> (M a) = M $ f <*> a

getMaxDepth :: M Int
getMaxDepth = M ask

getCounter :: M Int
getCounter = M get

update :: M ()
update = M $ modify pred

mChoose :: Random a => (a, a) -> M a
mChoose = M . lift . lift . lift . choose

mElements :: [a] -> M a
mElements = M . lift . lift . lift . elements

getWeight a maxA n m = prob a maxA (fromIntegral (1 + n) / fromIntegral (2 + m))

permutation :: [Double] -> M [Int]
permutation xs = iterate (zip xs [0..])

iterate :: [(Double, a)] -> M [a]
iterate [(_, x)] = return [x]
iterate xs = do
  (i, r) <- frequencyList xs
  rs <- iterate $ take i xs ++ drop (i + 1) xs
  return (r:rs)

frequencyList :: [(Double, a)] -> M (Int, a)
frequencyList xs0 = mChoose (0, tot) >>= (\r -> return $ pick r xs0 0)
  where tot = sum (map fst xs0)
        pick n ((k,x):xs) i
          | n <= k    = (i, x)
          | otherwise = pick (n-k) xs (i+1)

mToMaybe :: Int -> Int -> M Expr -> Gen MaybeExpr
mToMaybe bound maxD = fmap MaybeExpr . flip runReaderT maxD .  flip evalStateT bound . runMaybeT . unM
