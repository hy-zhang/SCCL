{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Base_GenCombinators where

import Prelude hiding (succ)
import qualified Test.QuickCheck.Gen as G
import Data.Proxy
import Data.Maybe
import System.Random
import Control.Exception (assert)
import Debug.Trace (trace)

import Base
import Base_RandMonad
import Base_WeightMonad
import Base_MonadTrans

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Data.Functor.Identity

-- Bad Combinator

or :: CoAlgM G.Gen f a -> CoAlgM G.Gen g a -> CoAlgM G.Gen (f :+: g) a
or cF cG a = G.frequency [(1, fmap Inl $ cF a), (1, fmap Inr $ cG a)]

-- I. Randomness. Uniform Distribution.

type Uniform = RandT Identity

uniformTrafo :: (Cardinality f, Cardinality g) => Trafo Uniform f g
uniformTrafo (Prod (Comp fs) (Comp gs)) =
  Comp $ frequency [(card (getProxy fs), fmap Inl fs),
                    (card (getProxy gs), fmap Inr gs)]
  where getProxy :: m (h a) -> Proxy h
        getProxy _ = Proxy

infixr |*|
(|*|) :: (Cardinality f, Cardinality g) => CoAlgM Uniform f a -> CoAlgM Uniform g a -> CoAlgM Uniform (f :+: g) a
(|*|) cF cG a = frequency [(getCard cF, fmap Inl $ cF a), (getCard cG, fmap Inr $ cG a)]

generateU :: Traversable f => a -> CoAlgM Uniform f a -> IO (Fix f)
generateU input coalg = newStdGen >>= runUniform
  where runUniform g = return . runIdentity . flip evalStateT g .
                       unRandT $ unfoldM coalg input


-- II. Failure. Weighted Random Distribution over Successful Generators.

type Weighted = MaybeT (WeightT (RandT Identity))

weightedTrafo :: Cardinality g => Trafo Weighted f g
weightedTrafo (Prod (Comp fs) (Comp gs)) = Comp . MaybeT $ do
  p  <- pointer
  rF <- runMaybeT $ fmap Inl fs
  rG <- runMaybeT $ succ >> fmap Inr gs
  when (card (getProxy gs) == 1 && isJust rG) (getWeight (p + 1) >>= setAcc)
  wF <- getWeight p
  wG <- getAcc
  case (rF, rG) of
    (Just _, Just _) -> setAcc (wF + wG) >> binomial (wF, return rF) (wG, return rG)
    (Just _, _)      -> setAcc wF >> return rF
    (_, Just _)      -> return rG
    _                -> return Nothing
  where getProxy :: m (h a) -> Proxy h
        getProxy _ = Proxy

infixr |**|
(|**|) :: Cardinality g => CoAlgM Weighted f a -> CoAlgM Weighted g a -> CoAlgM Weighted (f :+: g) a
(|**|) cF cG a = MaybeT $ do
  p  <- pointer
  rF <- runMaybeT $ fmap Inl (cF a)
  rG <- runMaybeT $ succ >> fmap Inr (cG a)
  when (getCard cG == 1 && isJust rG) (getWeight (p + 1) >>= setAcc)
  wF <- getWeight p
  wG <- getAcc
  case (rF, rG) of
    (Just _, Just _) -> setAcc (wF + wG) >> binomial (wF, return rF) (wG, return rG)
    (Just _, _     ) -> setAcc wF >> return rF
    (_     , Just _) -> return rG
    _                -> return Nothing
  
resetW :: Weighted a -> Weighted a
resetW x = reset >> x

generateW :: (Cardinality f, Traversable f) => [Double] -> a -> CoAlgM Weighted f a -> IO (Fix f)
generateW ws input coalg = assert (getCard coalg == length ws) $
    newStdGen >>= runWeighted
  where runWeighted g = return . fromJust . runIdentity . evalRandT g .
                        evalWeightT (0, 0) ws . runMaybeT $
                        unfoldM (resetW . coalg) input

-- III. Failure. Arity + Depth => Weight. Binomial distribution.
-- n = max arity.
-- k = this arity.
-- p = (1 + this depth) / (2 + max depth). (depth: 0-based)

type MaxArity = Int
type MaxDepth = Int
type ReadEnv = (MaxArity, MaxDepth)

type Acc = Double
type Bound = Int
type StateEnv = (Acc, Bound)

type Dynamic = MaybeT (StateT StateEnv (ReaderT ReadEnv (RandT Identity)))

infixr |***| -- Arities can be customized.
(|***|) :: (Cardinality g, Arity f, Arity g, Derive a Int) =>
    CoAlgM Dynamic f a -> CoAlgM Dynamic g a -> CoAlgM Dynamic (f :+: g) a
(|***|) cF cG a = MaybeT $ do
  env <- ask
  rF <- runMaybeT $ fmap Inl (cF a)
  rG <- runMaybeT $ fmap Inr (cG a)
  when (getCard cG == 1 && isJust rG) $ setAcc $ weight env (arityG, thisDepth)
  let wF = weight env (arityF, thisDepth)
  wG <- getAcc
  case (rF, rG) of
    (Just _, Just _) -> setAcc (wF + wG) >> binomial (wF, return rF)
                                                     (wG, return rG)
    (Just _, _)      -> setAcc wF >> return rF
    (_, Just _)      -> return rG
    _                -> return Nothing
  where arityF    = getArity cF
        arityG    = getArity cG
        thisDepth = derive a
        setAcc s  = modify $ \(_, c) -> (s, c) 
        getAcc    = get >>= return . fst

weight :: (Int, Int) -> (Int, Int) -> Double
weight (maxArity, maxDepth) (thisArity, thisDepth) =
  prob thisArity maxArity (fromIntegral (1 + thisDepth) / fromIntegral (2 + maxDepth))



resetD :: Dynamic a -> Dynamic a
resetD x = do
  (_, c) <- get
  if c <= 0 then none
            else put (0, c - 1) >> x

generateD :: (Arity f, Traversable f, Derive a Int) => Int -> a -> CoAlgM Dynamic f a -> IO (Maybe (Fix f))
generateD bound input coalg = newStdGen >>= runDynamic
  where res      = unfoldM (resetD . coalg) input
        maxArity = getArity coalg
        maxDepth = derive input
        env      = (maxArity, maxDepth)
        initS    = (0, bound)
        runDynamic g  = return . runIdentity . evalRandT g . flip runReaderT env . flip evalStateT initS . runMaybeT $ res

-- IV/V. MRM. (Future Work)
-- Failure. Binomial distribution: n = #permutations, p = current depth / (1 + max depth).

-- type family ToBSum (fs :: [* -> *]) where
  -- ToBSum '[f] = f
  -- ToBSum (f ': gs) = f :+: ToBSum gs

-- type G3 = G2

-- permutation :: Int -> [[Int]]
-- permutation n = perm [1..n]
  -- where perm :: [Int] -> [[Int]]
        -- perm [x] = [[x]]
        -- perm xs = concatMap (\i -> map ((xs !! i):) . perm $ del i) [0..n-1]
          -- where n   = length xs
                -- del = \i -> take i xs ++ drop (i + 1) xs

-- (|***|) :: Coalgs G2 fs a -> a -> G2 (ToBSum fs a)
-- (|***|) coalgs a = undefined