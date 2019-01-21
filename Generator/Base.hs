{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module Base where

import Control.Monad hiding (foldM)
import Data.Proxy

-- Fixpoint Type

data Fix f = In {out :: f (Fix f)}
instance Eq (f (Fix f)) => Eq (Fix f) where
  (In x) == (In y) = x == y
-- instance Show (f (Fix f)) => Show (Fix f) where
   -- show (In s) = show s

-- Sum Type
   
infixr :+:
data (f :+: g) a = Inl (f a) | Inr (g a) deriving (Functor, Foldable, Traversable, Eq)

instance (Show (f a), Show (g a)) => Show ((f :+: g) a) where
   show (Inl l) = show l
   show (Inr r) = show r
   
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  proj :: sup a -> Maybe (sub a)
instance {-# OVERLAPPABLE #-} Functor f => f :<: f where
  inj = id
  proj = Just
instance {-# OVERLAPPABLE #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  proj (Inl e) = Just e
  proj _ = Nothing
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  proj (Inr e) = proj e
  proj _ = Nothing

infixr <+>
(<+>) :: Alg f a -> Alg g a -> Alg (f :+: g) a
(f <+> g) (Inl x) = f x
(f <+> g) (Inr x) = g x

-- Product Type

data (f :*: g) a = Prod (f a) (g a) deriving Functor

(<**>) :: CoAlg f a -> CoAlg g a -> CoAlg (f :*: g) a
c1 <**> c2 = \x -> Prod (c1 x) (c2 x)

-- Composition of Functors

data (f :.: g) a = Comp { unComp :: f (g a) } deriving Functor
  
-- Algebras, Coalgebras and Transformations
  
type Alg f a = f a -> a

fold :: Functor f => Alg f a -> Fix f -> a
fold alg = alg . fmap (fold alg) . out
   
type CoAlg f a = a -> f a

unfold :: Functor f => CoAlg f a -> a -> Fix f
unfold coalg = In . fmap (unfold coalg) . coalg

type AlgM m f a = f a -> m a

foldM :: (Traversable f, Monad m) => AlgM m f a -> Fix f -> m a
foldM f = f <=< mapM (foldM f) . out

type CoAlgM m f a = a -> m (f a)

unfoldM :: (Monad m, Traversable f) => CoAlgM m f a -> a -> m (Fix f)
unfoldM h = fmap In . (mapM (unfoldM h) <=< h)

type Trafo m f g = forall a. ((m :.: f) :*: (m :.: g)) a -> (m :.: (f :+: g)) a

build :: (Monad m, Traversable f, Traversable g) => CoAlg (m :.: f) a -> CoAlg (m :.: g) a -> Trafo m f g ->
    a -> m (Fix (f :+: g))
build cF cG t = unfoldM (unComp . out) . unfold (t . out) . unfold (cF <**> cG)

-- Library for Generators

-- Cardinality

class Cardinality (f :: * -> *) where
  card :: Proxy f -> Int

instance {-# OVERLAPPABLE #-} (Cardinality f, Cardinality g) => Cardinality (f :+: g) where
  card _ = card (Proxy :: Proxy f) + card (Proxy :: Proxy g)
instance {-# OVERLAPPABLE #-} Cardinality f where
  card _ = 1

getCard :: Cardinality h => CoAlgM m h a -> Int
getCard = card . proxy

proxy :: CoAlgM m h a -> Proxy h
proxy _ = Proxy

-- Arity

class Arity (f :: * -> *) where
  arity :: Proxy f -> Int

instance (Arity f, Arity g) => Arity (f :+: g) where
  arity _ = max (arity (Proxy :: Proxy f)) (arity (Proxy :: Proxy g))

getArity :: Arity h => CoAlgM m h a -> Int
getArity = arity . proxy

-- Derive
class Derive a b | a -> b where
  derive :: a -> b

-- Binomial distribution

fact :: Int -> Int
fact 1 = 1
fact n = n * fact (n - 1)

prob :: Int -> Int -> Double -> Double
prob k n p = fromIntegral (choose n k) * pow p k * pow (1 - p) (n - k)
  where choose :: Int -> Int -> Int
        choose n 0 = 1
        choose 0 k = 0
        choose n k = choose (n - 1) (k - 1) * n `div` k
        pow :: Double -> Int -> Double
        pow x 0 = 1
        pow x n = x * pow x (n - 1)