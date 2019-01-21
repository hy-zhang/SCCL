{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Base_MonadTrans where

import Prelude hiding (succ)
import Base_WeightMonad
import Base_RandMonad
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer

-- WeightT

instance MonadTrans WeightT where
  lift = WeightT . lift . lift
  
instance MonadWeight m => MonadWeight (MaybeT m) where
  pointer   = lift pointer
  succ      = lift succ
  reset     = lift reset
  getWeight = lift . getWeight
  getAcc    = lift getAcc
  setAcc    = lift . setAcc

instance MonadWeight m => MonadWeight (RandT m) where
  pointer   = lift pointer
  succ      = lift succ
  reset     = lift reset
  getWeight = lift . getWeight
  getAcc    = lift getAcc
  setAcc    = lift . setAcc

-- RandT

instance MonadTrans RandT where
  lift = RandT . lift

instance MonadRand m => MonadRand (MaybeT m) where
  seed = MaybeT . liftM Just $ seed

instance MonadRand m => MonadRand (WeightT m) where
  seed = lift seed

instance MonadRand m => MonadRand (StateT s m) where
  seed = lift seed

instance MonadRand m => MonadRand (ReaderT s m) where
  seed = lift seed

instance (Monoid s, MonadRand m) => MonadRand (WriterT s m) where
  seed = lift seed

-- MonadMaybe

class Monad m => MonadMaybe m where
  none :: m a

instance Monad m => MonadMaybe (MaybeT m) where
  none = MaybeT $ return Nothing

instance MonadMaybe m => MonadMaybe (RandT m) where
  none = lift none