{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies #-}

module Base where

import Control.Monad hiding (foldM)

-- Fixpoint Type

data Fix f = In {out :: f (Fix f)}
instance Eq (f (Fix f)) => Eq (Fix f) where
  (In x) == (In y) = x == y
instance Show (f (Fix f)) => Show (Fix f) where
   show (In s) = show s

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

-- Basic Combinators

-- comp1 :: CoAlgM (State StdGen) f a -> CoAlgM (State StdGen) g a -> CoAlgM (State StdGen) (f :+: g) a
-- comp1 cF cG = \x -> unComp . randomSelect $ Prod (Comp $ cF x) (Comp $ cG x)

-- comp2 :: CoAlgM [] f a -> CoAlgM [] g a -> CoAlgM [] (f :+: g) a
-- comp2 cF cG = \x -> map Inl (cF x) ++ map Inr (cG x)

-- comp3 :: CoAlgM Maybe f a -> CoAlgM Maybe g a -> CoAlgM Maybe (f :+: g) a
-- comp3 cF cG = \x -> fmap Inl (cF x) `mplusMaybe` fmap Inr (cG x)
  -- where Just x  `mplusMaybe` _ = Just x
        -- Nothing `mplusMaybe` y = y
        
-- comp4 = undefined

-- Bad Combinator for Random Generation

-- randomSelect :: Trafo (State StdGen) f g
-- randomSelect (Prod (Comp rf) (Comp rg)) = Comp . withRand $ choose res1 res2
  -- where (res1, res2) = (fmap Inl rf, fmap Inr rg)
        -- withRand :: (Double -> State StdGen a) -> State StdGen a
        -- withRand f = get >>= \g -> let (r, g') = random g in put g' >> f r
        -- choose :: a -> a -> Double -> a
        -- choose x1 x2 r  | r < 0.5    = x1
                        -- | otherwise  = x2

-- Similar for other mts.

