{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 PatternSynonyms, ViewPatterns #-}

module EvalMonad where

import Base
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Except

-- Library of Evaluation

-- 1. Definition of Error Type, and Monad Transformer
data EvalError = NoRulesApplied | ConflictReduction | ConflictCongruence deriving Show

type IsDone = Bool
type IsCongruenceRule = Bool
type EvalState = (IsDone, IsCongruenceRule)

newtype EvalT m a = EvalT {
  unEvalT :: StateT EvalState (ExceptT EvalError m) a 
} deriving Functor

-- 2. Instances of Applicative and Monad
instance Monad m => Monad (EvalT m) where
  return = EvalT . return
  (EvalT m) >>= f = EvalT $ m >>= unEvalT . f

instance (Functor f, Monad f) => Applicative (EvalT f) where
  pure = return
  (<*>) = ap

-- 3. Type Class for Monad Transformer
class Monad m => MonadEval m where
  cgrRule :: a -> m a
  rdcRule :: a -> m a
  noMatch :: m a
  catchNoMatch :: m a -> m a -> m a
  ifDone :: m a -> m a -> m a

instance Monad m => MonadEval (EvalT m) where
  cgrRule x = EvalT $ get >>= \(a, _) -> put (a, True)  >> return x
  rdcRule x = EvalT $ get >>= \(a, _) -> put (a, False) >> return x
  noMatch   = EvalT . lift $ throwError NoRulesApplied
  catchNoMatch x y = EvalT $ StateT $ \s -> ExceptT $ do
    r <- runEval s x
    case r of
      Left NoRulesApplied -> runEval s y
      a -> return a
  ifDone x y = EvalT $ StateT $ \s -> ExceptT $ do
    if fst s then runEval s x else do
      r <- runEval s y
      case r of
        Right (r', (a, False)) -> return $ Right (r', (True, False))
        a -> return a

runEval :: EvalState -> EvalT m a -> m (Either EvalError (a, EvalState))
runEval s = runExceptT . flip runStateT s . unEvalT

compose :: Monad m => EvalT m a -> EvalT m a -> EvalT m a
compose x y = EvalT $ StateT $ \s -> ExceptT $ do
  rx <- runEval s x
  case rx of
    Left NoRulesApplied -> runEval s y
    Left e -> return $ Left e
    resX@(Right (rx', (a, bx))) -> do
      ry <- runEval s y
      case ry of
        Left NoRulesApplied -> return resX
        Left e -> return $ Left e
        resY@(Right (ry', (a', by))) -> case (bx, by) of
          (False, True)  -> return resX
          (True, False)  -> return resY
          (False, False) -> return $ Left ConflictReduction
          (True, True)   -> return $ Left ConflictCongruence
