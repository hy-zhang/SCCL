{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures #-}

module Properties where

import qualified GenExpr_Typed_DepthSize as T
import qualified Base_GenCombinators as C

import Base
import Base_WeightMonad
import Base_RandMonad
import System.Random
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Data.Functor.Identity
import AST
import AST_Print
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Maybe (isJust, isNothing, fromJust)
import AST_TypeCheck
import AST_Eval
import AST_IsBounded

-- Deforestation.

cF = Comp . T.gLit
cG = Comp . T.gAdd

genA :: T.TypedSeed -> C.Weighted (Fix (LitF :+: AddF))
genA = unfoldM (unComp . out) . unfold (Comp . C.resetW . unComp . out) . unfold (C.weightedTrafo . out) . unfold (cF <**> cG)

genB :: T.TypedSeed -> C.Weighted (Fix (LitF :+: AddF))
genB = unfoldM (C.resetW . unComp . C.weightedTrafo . (cF <**> cG))

genC :: T.TypedSeed -> C.Weighted (Fix (LitF :+: AddF))
genC = unfoldM (C.resetW . (T.gLit C.|**| T.gAdd))

deforest :: Int -> Int -> Bool
deforest n g = rA == rB && rB == rC
  where n' = n `mod` 10
        g' = mkStdGen g
        rA = runGen genA
        rB = runGen genB
        rC = runGen genC
        runGen :: (T.TypedSeed -> C.Weighted a) -> Maybe a
        runGen gen = runIdentity . evalRandT g' . evalWeightT (0,0) [1,1] . runMaybeT $ gen (n', Nothing, [])

runDeforest = quickCheckWith (stdArgs {maxSize = 1000000}) deforest

data MaybeExpr f = MaybeExpr { unMaybe :: Maybe (Fix f) }
instance Show (Fix f) => Show (MaybeExpr f) where
  show = show . unMaybe

instance (Functor f, Traversable f, Arity f, T.RandomD f T.TypedSeed) => Arbitrary (MaybeExpr f) where
  arbitrary = do
    n <- getSize
    seed <- chooseAny
    -- let n     = max 5 size
    let coalg = T.genD :: CoAlgM C.Dynamic f T.TypedSeed
    let res   = unfoldM (C.resetD . coalg) (n, Nothing, [])
    let env   = (getArity coalg, n)
    let initS = (0, 1000)
    let g     = mkStdGen seed
    return . MaybeExpr . runIdentity . evalRandT g . flip runReaderT env . flip evalStateT initS . runMaybeT $ res

preserveType :: MaybeExpr LNG -> Property
preserveType (MaybeExpr expr) = isJust expr ==> isJust t && t == t'
  where e  = fromJust expr
        t  = typeCheck e
        t' = typeCheck (evaluate e)

runPreserveType = quickCheckWith (stdArgs {maxSize = 30}) preserveType

bounded :: MaybeExpr LNG -> Property
bounded (MaybeExpr expr) = isJust expr ==> isBounded (evaluate e)
  where e = fromJust expr

runBounded = quickCheckWith (stdArgs {maxSize = 30}) bounded

