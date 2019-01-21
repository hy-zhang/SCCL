{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module GenExpr_DepthSize where

import Base
import Base_MonadTrans
import Base_RandMonad
import Base_GenCombinators

import AST
import AST_BFS
import AST_Print

import Data.Maybe (fromJust)

-- Size = depth.
type Size = Int
type MaxId = Int
type Seed = (Size, MaxId)

when :: MonadMaybe m => Bool -> m a -> m a
when True x = x
when _ _    = none

liftS :: (Functor f, Functor m) => CoAlgM m f Size -> CoAlgM m f Seed
liftS c (size, max) = fmap (fmap (\x -> (x, max))) (c size)

gLit :: (MonadMaybe m, MonadRand m) => CoAlgM m LitF Seed
gLit = liftS gLit'
  where gLit' n = when (n <= 0) [Lit x | x <- choose (0, 100)]

gAdd :: (MonadMaybe m, MonadRand m) => CoAlgM m AddF Seed
gAdd = liftS $ \n -> if n <= 0 then none else do
  n' <- choose (0, n - 1)
  (nA, nB) <- elements [(n - 1, n'), (n', n - 1)]
  return $ Add nA nB
            
gMul :: (MonadMaybe m, MonadRand m) => CoAlgM m MulF Seed
gMul = liftS gMul'
  where gMul' n = when (n > 0) [Mul nA nB |  n'      <- choose (0, n - 1),
                                            (nA, nB) <- elements [(n - 1, n'), (n', n - 1)]]

gBool :: (MonadMaybe m, MonadRand m) => CoAlgM m BoolF Seed
gBool = liftS gBool'
  where gBool' n = when (n <= 0) [BoolV x | x <- choose (False, True)]

gIf :: (MonadMaybe m, MonadRand m) => CoAlgM m IfF Seed
gIf = liftS gIf'
  where gIf' n = when (n > 0) [If nA nB nC |  n1          <- choose (0, n - 1),
                                              n2          <- choose (0, n - 1),
                                             (nA, nB, nC) <- elements [(n - 1, n1, n2), (n1, n - 1, n2), (n1, n2, n - 1)]]

gEqual :: (MonadMaybe m, MonadRand m) => CoAlgM m EqualF Seed
gEqual = liftS gEqual'
  where gEqual' n = when (n > 0) [Equal nA nB |  n'      <- choose (0, n - 1),
                                                (nA, nB) <- elements [(n - 1, n'), (n', n - 1)]]

gVar :: (MonadMaybe m, MonadRand m) => CoAlgM m VarF Seed
gVar (n, max) = when (n <= 0 && max >= 0) [Var x | x <- choose (0, max)]

gLam :: (MonadMaybe m, MonadRand m) => CoAlgM m LamF Seed
gLam (n, max) = when (n > 0) [Lam t (n - 1, max + 1) | t <- gType]

gApp :: (MonadMaybe m, MonadRand m) => CoAlgM m AppF Seed
gApp = liftS gApp'
  where gApp' n = when (n > 0) [App nA nB |  n'      <- choose (0, n - 1),
                                            (nA, nB) <- elements [(n - 1, n'), (n', n - 1)]]

gType :: MonadRand m => m Type
gType = [t | t1 <- elements [TBool, TLit],
             t2 <- elements [TBool, TLit],
             t  <- elements [TBool, TLit, TFunc t1 t2]]

--Weighted.
coalgsWeighted :: CoAlgM Weighted LNG Seed
coalgsWeighted = gLit |**| gAdd |**| gMul |**| gBool |**| gIf |**| gEqual |**| gVar |**| gLam |**| gApp

-- r2 :: Size -> IO ()
-- r2 n = generateWeighted (replicate 9 1) (n, -1) coalgsWeighted >>= printBFS

r2 :: Size -> IO (Fix LNG)
r2 n = generateWeighted (replicate 9 1) (n, -1) coalgsWeighted

--G3. Makes more sense with depth size.
instance Derive Seed Int where derive = fst

coalgsG3 :: CoAlgM G3 LNG Seed
coalgsG3 = gLit |***| gAdd |***| gMul |***| gBool |***| gIf |***| gEqual |***| gVar |***| gLam |***| gApp

-- r3 :: Size -> IO ()
-- r3 n = generateG3 100000 (n, -1) coalgsG3 >>= \e -> case e of
  -- Just e0 -> printBFS e0
  -- Nothing -> return ()

r3MayFail :: Size -> IO (Fix LNG)
r3MayFail n = fmap fromJust $ generateG3 100000 (n, -1) coalgsG3
