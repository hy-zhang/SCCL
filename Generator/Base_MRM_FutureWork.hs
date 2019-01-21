{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Base_MRM_FutureWork where

import Base
import Base_RandMonad
import Base_MonadTrans
import AST
import qualified GenExpr_Typed_DepthSize as T

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Reader
import System.Random
import Data.Functor.Identity

type Arities = [Int]
type MaxArity = Int
type MaxDepth = Int
type ReadEnv = (Arities, MaxArity, MaxDepth)
type Counter = Int
type G4 = MaybeT (StateT Counter (ReaderT ReadEnv (RandT Identity)))

data CoAlgs (m :: * -> *) (fs :: [* -> *]) (a :: *) where
  Nil  :: CoAlgs m '[] a
  Cons :: Arity f => CoAlgM m f a -> CoAlgs m fs a -> CoAlgs m (f ': fs) a

class Collect (fs :: [* -> *]) (gs :: * -> *) where
  collect :: Monad m => CoAlgs m fs a -> [CoAlgM m gs a]

instance Collect '[f] f where
  collect (Cons c Nil) = [c]

instance Collect fs gs => Collect (f ': fs) (f :+: gs) where
  collect (Cons c cs) = (fmap Inl . c) : map (\c -> fmap Inr . c) (collect cs)

getWeights :: ReadEnv -> Int -> [Double]
getWeights (ws, maxW, maxD) d = map (\a -> binom maxW a (fromIntegral (1 + d) / fromIntegral (2 + maxD))) ws

-- TODO: derivation size.
compose :: Derive a Int => (CoAlgs G4 fs a -> [CoAlgM G4 gs a]) -> CoAlgs G4 fs a -> CoAlgM G4 gs a
compose collect coalgs a = do
  counter <- get
  when (counter <= 0) none
  put $ counter - 1
  env <- ask
  let ws = getWeights env (derive a)
  order <- permutation ws
  let cList = collect coalgs
  foldl1 (\x y input -> x input `mplus` y input) (map (cList !!) order) a
  where permutation :: [Double] -> G4 [Int]
        permutation xs = iterate (zip xs [0..])
        iterate :: [(Double, a)] -> G4 [a]
        iterate [(_, x)] = return [x]
        iterate xs = do
          (i, r) <- frequencyList xs
          rs <- iterate $ take i xs ++ drop (i + 1) xs
          return (r:rs)
        frequencyList :: [(Double, a)] -> G4 (Int, a)
        frequencyList xs0 = choose (0, tot) >>= (\r -> return $ pick r xs0 0)
          where tot = sum (map fst xs0)
                pick n ((k,x):xs) i
                  | n <= k    = (i, x)
                  | otherwise = pick (n-k) xs (i+1)

gens :: CoAlgM G4 LNG T.TypedSeed
gens = compose collect (Cons T.gLit $ Cons T.gAdd $ Cons T.gMul $
                Cons T.gBool $ Cons T.gIf $ Cons T.gEqual $
                Cons T.gVar $ Cons T.gLam $ Cons T.gApp $ Nil)

generateG4 :: (Functor f, Traversable f, Derive a Int) => Int -> [Int] -> a -> CoAlgM G4 f a -> IO (Maybe (Fix f))
generateG4 bound arities input coalg = newStdGen >>= runG4
  where res      = unfoldM coalg input
        env      = (arities, foldl1 max arities, derive input)
        runG4 g  = return . runIdentity . evalRandT g . flip runReaderT env . flip evalStateT bound . runMaybeT $ res

r4 :: Int -> IO (Maybe (Fix LNG))
r4 n = generateG4 100000 [0,2,2,0,3,2,0,1,2] (n, Nothing, []) gens