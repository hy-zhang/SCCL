{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Base_WeightMonad where -- Note: can be used for both weight-fixed and weight-adjusted combinator.

import Prelude hiding (succ)
import Control.Monad.State.Lazy
import Control.Monad.Reader

-- Library of Weight with Adjustness

-- 1. Definition of Monad Transformer
newtype WeightT m a = WeightT { unWeightT :: StateT (Int, Double) (ReaderT [Double] m) a } deriving Functor

evalWeightT :: Monad m => (Int, Double) -> [Double] -> WeightT m a -> m a
evalWeightT s r = flip runReaderT r . flip evalStateT s . unWeightT

-- 2. Instances of Applicative and Monad
instance Monad m => Monad (WeightT m) where
  return = WeightT . return
  (WeightT m) >>= f = WeightT $ m >>= unWeightT . f

instance (Functor f, Monad f) => Applicative (WeightT f) where
  pure = WeightT . pure
  (WeightT f) <*> (WeightT a) = WeightT $ f <*> a

-- 3. Type Class for Monad Transformer
class Monad m => MonadWeight m where
  pointer   :: m Int
  succ      :: m ()
  reset     :: m ()
  getWeight :: Int -> m Double
  getAcc    :: m Double
  setAcc    :: Double -> m ()

instance Monad m => MonadWeight (WeightT m) where
  pointer     = WeightT $ get >>= return . fst
  succ        = WeightT $ modify $ \(p, acc) -> (p + 1, acc)
  reset       = WeightT $ put (0, 0)
  getWeight p = WeightT $ ask >>= return . (!! p)
  getAcc      = WeightT $ get >>= return . snd
  setAcc w    = WeightT $ modify $ \(p, _) -> (p, w)
