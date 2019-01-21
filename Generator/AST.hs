{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions, PatternSynonyms, ViewPatterns #-}

module AST where

import Base

-- Datatypes

data Type = TLit | TBool | TFunc Type Type deriving Eq

instance Show Type where
  show TBool = "Bool"
  show TLit = "Lit"
  show (TFunc t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

data LitF   e = Lit Int        deriving (Eq, Functor, Foldable, Traversable)
data AddF   e = Add e e        deriving (Eq, Functor, Foldable, Traversable)
data MulF   e = Mul e e        deriving (Eq, Functor, Foldable, Traversable)
data BoolF  e = BoolV Bool     deriving (Eq, Functor, Foldable, Traversable)
data IfF    e = If e e e       deriving (Eq, Functor, Foldable, Traversable)
data EqualF e = Equal e e      deriving (Eq, Functor, Foldable, Traversable)
data VarF   e = Var Int        deriving (Eq, Functor, Foldable, Traversable)
data LamF   e = Lam Type e     deriving (Eq, Functor, Foldable, Traversable)
data AppF   e = App e e        deriving (Eq, Functor, Foldable, Traversable)

type LNG = LitF :+: AddF :+: MulF :+: BoolF :+: IfF :+: EqualF :+: VarF :+: LamF :+: AppF

-- Arities

instance Arity LitF   where arity _ = 0
instance Arity AddF   where arity _ = 2
instance Arity MulF   where arity _ = 2
instance Arity BoolF  where arity _ = 0
instance Arity IfF    where arity _ = 3
instance Arity EqualF where arity _ = 2
instance Arity VarF   where arity _ = 0
instance Arity LamF   where arity _ = 1
instance Arity AppF   where arity _ = 2

-- Smart constructors

lit :: LitF :<: f => Int -> Fix f
lit = In . inj . Lit

add :: AddF :<: f => Fix f -> Fix f -> Fix f
add x y = In . inj $ Add x y

mul :: MulF :<: f => Fix f -> Fix f -> Fix f
mul x y = In . inj $ Mul x y

boolV :: BoolF :<: f => Bool -> Fix f
boolV = In . inj . BoolV

ifC :: IfF :<: f => Fix f -> Fix f -> Fix f -> Fix f
ifC x y z = In . inj $ If x y z

equal :: EqualF :<: f => Fix f -> Fix f -> Fix f
equal x y = In . inj $ Equal x y

var :: VarF :<: f => Int -> Fix f
var = In . inj . Var

lam :: LamF :<: f => Type -> Fix f -> Fix f
lam t e = In . inj $ Lam t e

app :: AppF :<: f => Fix f -> Fix f -> Fix f
app x y = In . inj $ App x y

-- Projections

pattern LitP x     <- (proj . out -> Just (Lit x))
pattern AddP x y   <- (proj . out -> Just (Add x y))
pattern MulP x y   <- (proj . out -> Just (Mul x y))
pattern BoolP x    <- (proj . out -> Just (BoolV x))
pattern IfP x y z  <- (proj . out -> Just (If x y z))
pattern EqualP x y <- (proj . out -> Just (Equal x y))
pattern VarP x     <- (proj . out -> Just (Var x))
pattern LamP t x   <- (proj . out -> Just (Lam t x))
pattern AppP x y   <- (proj . out -> Just (App x y))

-- Output distributions.

outputDistribution :: Int -> IO ()
outputDistribution depth = mapM_ (\d -> putStrLn $ "depth = " ++ show d ++ ": " ++ show (map (trunc 3 . f d) [0..3])) depths
  where trunc :: Int -> Double -> Double 
        trunc n x = let t = 10^n in (fromIntegral (floor (x * t))) / t
        depths = [depth,depth-1..0]
        f thisDepth thisArity = prob 3 thisArity (fromIntegral (1 + thisDepth) / fromIntegral (2 + depth))
