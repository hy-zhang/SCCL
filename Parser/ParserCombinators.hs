{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies #-}

module ParserCombinators where

import Base
import Data.Functor.Identity
import ParserMonad
import ParserAuxMonad

-- I. Choice.

type P = ParserT Identity

infixr <||>
(<||>) :: CoAlgM P f a -> CoAlgM P g a -> CoAlgM P (f :+: g) a
(<||>) cF cG a = fmap Inl (cF a) `choice` fmap Inr (cG a)

data FLit a = FLit Int
data FAdd a = FAdd Int a

pLit :: CoAlgM P FLit ()
pLit _ = liftM FLit int

pAdd :: CoAlgM P FAdd ()
pAdd _ = do n <- int
            char '+'
            return $ FAdd n ()
            