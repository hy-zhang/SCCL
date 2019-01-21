{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Base_RandMonad where

import Control.Monad.State.Lazy
import System.Random
  
-- Library of Randomness

-- 1. Definition of Monad Transformer
newtype RandT m a = RandT { unRandT :: StateT StdGen m a } deriving Functor

evalRandT :: Monad m => StdGen -> RandT m a -> m a
evalRandT g = flip evalStateT g . unRandT

-- 2. Instances of Applicative and Monad
instance Monad m => Monad (RandT m) where
  return = RandT . return
  (RandT m) >>= f = RandT $ m >>= unRandT . f

instance (Functor f, Monad f) => Applicative (RandT f) where
  pure = RandT . pure
  (RandT f) <*> (RandT a) = RandT $ f <*> a

-- 3. Type Class for Monad Transformer
class Monad m => MonadRand m where
  seed :: m StdGen
  choose :: Random a => (a, a) -> m a
  choose range = liftM (fst . randomR range) seed
  elements :: [a] -> m a
  elements [] = error "elements applied to empty list"
  elements xs = (xs !! ) `fmap` choose (0, length xs - 1)
  frequency :: [(Int, m a)] -> m a
  frequency [] = error "frequency applied to empty list"
  frequency xs = choose (1, total) >>= (`pick` xs)
    where total = sum (map fst xs)
          pick n ((k, x):ys) | n <= k    = x
                             | otherwise = pick (n - k) ys
  binomial :: (Double, m a) -> (Double, m a) -> m a
  binomial (w1, x) (w2, y) = choose (0, 1) >>= \r -> if r < w1 / (w1 + w2) then x else y

-- 4. Instances of Monad Transformer class 
instance Monad m => MonadRand (RandT m) where
  seed = RandT $ do s <- get
                    let (s1, s2) = split s
                    put s2
                    return s1
