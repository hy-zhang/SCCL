{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module GenExpr_ConstrSize where

import Base
import Base_MonadTrans
import Base_RandMonad
import Base_GenCombinators

import AST
import AST_Print
import AST_BFS
import AST_Depth
import qualified AST_NonModular as N

import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as G
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

-- Size = #internal constructors.
type Size = Int
type MaxId = Int
type Seed = (Size, MaxId)

when :: MonadMaybe m => Bool -> m a -> m a
when True x = x
when _ _    = none

liftS :: (Functor f, Functor m) => CoAlgM m f Size -> CoAlgM m f Seed
liftS c (size, max) = fmap (fmap (\x -> (x, max))) (c size)

gLit :: (MonadMaybe m, MonadRand m) => CoAlgM m LitF Seed
gLit = liftS $ \n -> if n > 0 then none else do
  x <- choose (0, 100)
  return $ Lit x

gAdd :: (MonadMaybe m, MonadRand m) => CoAlgM m AddF Seed
gAdd = liftS $ \n -> if n <= 0 then none else do
  n' <- choose (0, n - 1)
  return $ Add n' (n - 1 - n')

gMul :: (MonadMaybe m, MonadRand m) => CoAlgM m MulF Seed
gMul = liftS gMul'
  where gMul' n = when (n > 0) [Mul n' (n - 1 - n') | n' <- choose (0, n - 1)]

gBool :: (MonadMaybe m, MonadRand m) => CoAlgM m BoolF Seed
gBool = liftS gBool'
  where gBool' n = when (n <= 0) [BoolV x | x <- choose (False, True)]

gIf :: (MonadMaybe m, MonadRand m) => CoAlgM m IfF Seed
gIf = liftS gIf'
  where gIf' n = when (n > 0) [If nA nB (n - 1 - nA - nB) | nA <- choose (0, n - 1), nB <- choose (0, n - 1 - nA)]

gEqual :: (MonadMaybe m, MonadRand m) => CoAlgM m EqualF Seed
gEqual = liftS gEqual'
  where gEqual' n = when (n > 0) [Equal n' (n - 1 - n') | n' <- choose (0, n - 1)]

gVar :: (MonadMaybe m, MonadRand m) => CoAlgM m VarF Seed
gVar (n, max) = when (n <= 0 && max >= 0) [Var x | x <- choose (0, max)]

gLam :: (MonadMaybe m, MonadRand m) => CoAlgM m LamF Seed
gLam (n, max) = when (n > 0) [Lam t (n - 1, max + 1) | t <- gType]

gApp :: (MonadMaybe m, MonadRand m) => CoAlgM m AppF Seed
gApp = liftS gApp'
  where gApp' n = when (n > 0) [App n' (n - 1 - n') | n' <- choose (0, n - 1)]

gType :: MonadRand m => m Type
gType = [t | t1 <- elements [TBool, TLit],
             t2 <- elements [TBool, TLit],
             t  <- elements [TBool, TLit, TFunc t1 t2]]

-- G2. Fit Constr better.
coalgsG2 :: CoAlgM G2 LNG Seed
coalgsG2 = gLit |**| gAdd |**| gMul |**| gBool |**| gIf |**| gEqual |**| gVar |**| gLam |**| gApp

class (Functor f, Traversable f) => RandomW f a where genW :: CoAlgM G2 f a
instance (RandomW f a, RandomW g a, Cardinality g) => RandomW (f :+: g) a
  where genW = genW |**| genW
instance RandomW LitF   Seed where genW = gLit
instance RandomW AddF   Seed where genW = gAdd
instance RandomW MulF   Seed where genW = gMul
instance RandomW BoolF  Seed where genW = gBool
instance RandomW IfF    Seed where genW = gIf
instance RandomW EqualF Seed where genW = gEqual
instance RandomW VarF   Seed where genW = gVar
instance RandomW LamF   Seed where genW = gLam
instance RandomW AppF   Seed where genW = gApp

class (Functor f, Traversable f) => RandomD f a where genD :: CoAlgM G3 f a
instance (RandomD f a, RandomD g a, Cardinality g, Arity f, Arity g, Derive a Int) => RandomD (f :+: g) a
  where genD = genD |***| genD
instance RandomD LitF   Seed where genD = gLit
instance RandomD AddF   Seed where genD = gAdd
instance RandomD MulF   Seed where genD = gMul
instance RandomD BoolF  Seed where genD = gBool
instance RandomD IfF    Seed where genD = gIf
instance RandomD EqualF Seed where genD = gEqual
instance RandomD VarF   Seed where genD = gVar
instance RandomD LamF   Seed where genD = gLam
instance RandomD AppF   Seed where genD = gApp

r2 :: Size -> IO (Fix LNG)
r2 n = generateG2 (replicate 9 1) (n, -1) coalgsG2

r2' :: Size -> IO (Fix LNG)
r2' n = generateG2 (replicate 9 1) (n, -1) (genW :: CoAlgM G2 LNG Seed)

r3' :: Size -> IO (Maybe (Fix LNG))
r3' n = generateG3 10000 (n, -1) (genD :: CoAlgM G3 LNG Seed)

-- G3. Does not fit constr well.
instance Derive Seed Int where derive = fst

coalgsG3 :: CoAlgM G3 LNG Seed
coalgsG3 = gLit |***| gAdd |***| gMul |***| gBool |***| gIf |***| gEqual |***| gVar |***| gLam |***| gApp

r3 :: Size -> IO (Maybe String)
r3 n = fmap (fmap show) $ generateG3 100000 (n, -1) coalgsG3

r3MayFail :: Size -> IO (Fix LNG)
r3MayFail n = fmap fromJust $ generateG3 100000 (n, -1) coalgsG3

-- Non-modular QuickCheck generator for G2.
data Weights = Weights {
  wLit   :: Int, wAdd :: Int, wMul   :: Int,
  wBool  :: Int, wIf  :: Int, wEqual :: Int,
  wVar   :: Int, wLam :: Int, wApp   :: Int
}

instance Q.Arbitrary N.Expr where
  arbitrary = G.getSize >>= \n -> gen (n, -1)
    where ws = Weights 1 1 1 1 1 1 1 1 1
          gen :: Seed -> G.Gen N.Expr
          gen s@(n, max) | n <= 0 && max >= 0 = G.frequency $ (wVar ws, genVar s) : genLitBool s
                         | n <= 0             = G.frequency $ genLitBool s
                         | otherwise          = G.frequency $ genOthers s
          genLitBool s = [(wLit ws, genLit s), (wBool ws, genBool s)]
          genOthers  s = [(wAdd ws, genAdd s), (wMul ws, genMul s), (wIf ws, genIf s),
                          (wEqual ws, genEqual s), (wLam ws, genLam s), (wApp ws, genApp s)]
          genLit   (n, max) = [N.Lit x | x <- G.choose (0, 100)]
          genBool  (n, max) = [N.BoolV x | x <- G.choose (False, True)]
          genVar   (n, max) = [N.Var x | x <- G.choose (0, max)]
          genAdd   (n, max) = do n' <- G.choose (0, n - 1)
                                 eA <- gen (n', max)
                                 eB <- gen (n - 1 - n', max)
                                 return $ N.Add eA eB
          genMul   (n, max) = do n' <- G.choose (0, n - 1)
                                 eA <- gen (n', max)
                                 eB <- gen (n - 1 - n', max)
                                 return $ N.Mul eA eB
          genIf    (n, max) = do nA <- G.choose (0, n - 1)
                                 nB <- G.choose (0, n - 1 - nA)
                                 eA <- gen (nA, max)
                                 eB <- gen (nB, max)
                                 eC <- gen (n - 1 - nA - nB, max)
                                 return $ N.If eA eB eC
          genEqual (n, max) = do n' <- G.choose (0, n - 1)
                                 eA <- gen (n', max)
                                 eB <- gen (n - 1 - n', max)
                                 return $ N.Equal eA eB
          genLam   (n, max) = do t1 <- G.elements [N.TBool, N.TLit]
                                 t2 <- G.elements [N.TBool, N.TLit]
                                 t  <- G.elements [N.TBool, N.TLit, N.TFunc t1 t2]
                                 e  <- gen (n - 1, max + 1)
                                 return $ N.Lam t e
          genApp   (n, max) = do n' <- G.choose (0, n - 1)
                                 eA <- gen (n', max)
                                 eB <- gen (n - 1 - n', max)
                                 return $ N.App eA eB

-- FutureWork: Reuse coalgs.
--             Have a typed list [gLit, gAdd, ...]. Then work on it. (Check MRM paper for extensible constraints.)
-- Discussion: why do we need the binary combinator, instead of putting all coalgs in a list, and use ad-hoc
--             composition. Because we want to connect to the theory (natTrafo). But indeed we can generalize it
--             into MRM-style. (FutureWork)
