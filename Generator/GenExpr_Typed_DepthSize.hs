{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions #-}

module GenExpr_Typed_DepthSize where

import Data.List
import System.Random
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy hiding (when)
import Control.Monad.Reader hiding (when)
import Data.Maybe (fromJust)

import Base
import Base_MonadTrans
import Base_RandMonad
import Base_GenCombinators

import AST
import AST_Depth
import AST_BFS
import AST_Print
import qualified AST_NonModular as N

import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as G

-- Size = depth. Since size is a non-strict bound, does not make much sense to be #internal constructors.

type Size = Int
type Env = [Type]
type ExpectType = Maybe Type
type TypedSeed = (Size, ExpectType, Env)

when :: MonadMaybe m => Bool -> m a -> m a
when True x = x
when _ _    = none

liftS :: (Functor f, Functor m) => CoAlgM m f (Size, ExpectType) -> CoAlgM m f TypedSeed
liftS c (size, t, vars) = fmap (fmap (\(x, y) -> (x, y, vars))) (c (size, t))

gLit :: (MonadMaybe m, MonadRand m) => CoAlgM m LitF TypedSeed
gLit = liftS gLit'
  where gLit' (n, t) = when (t `elem` [Just TLit, Nothing])
            [Lit x | x <- choose (0, 100)]

gAdd :: (MonadMaybe m, MonadRand m) => CoAlgM m AddF TypedSeed
gAdd = liftS $ \(n, t) ->
  if t `notElem` [Just TLit, Nothing] then none else do
    let n0 = max 0 (n - 1)
    n' <- choose (0, n0)
    (nA, nB) <- elements [(n0, n'), (n', n0)]
    return $ Add (nA, Just TLit) (nB, Just TLit)

gMul :: (MonadMaybe m, MonadRand m) => CoAlgM m MulF TypedSeed
gMul = liftS gMul'
  where gMul' (n, t) = when (t `elem` [Just TLit, Nothing])
            [Mul (nA, Just TLit) (nB, Just TLit) |
                let n0 = max 0 (n - 1),
                n' <- choose (0, n0),
                (nA, nB) <- elements [(n0, n'), (n', n0)]]

gBool :: (MonadMaybe m, MonadRand m) => CoAlgM m BoolF TypedSeed
gBool = liftS gBool'
  where gBool' (n, t) = when (t `elem` [Just TBool, Nothing])
            [BoolV x | x <- choose (False, True)]

gIf :: (MonadMaybe m, MonadRand m) => CoAlgM m IfF TypedSeed
gIf = liftS gIf'
  where gIf' (n, t) = [If (nA, Just TBool) (nB, Just t') (nC, Just t') |
            let n0 = max 0 (n - 1),
            n1 <- choose (0, n0),
            n2 <- choose (0, n0),
            t' <- case t of Just t0 -> return t0
                            Nothing -> gType,
            (nA, nB, nC) <- elements [(n0, n1, n2), (n1, n0, n2), (n1, n2, n0)]]

gEqual :: (MonadMaybe m, MonadRand m) => CoAlgM m EqualF TypedSeed
gEqual = liftS gEqual'
  where gEqual' (n, t) = when (t `elem` [Just TBool, Nothing])
            [Equal (nA, Just t') (nB, Just t') |
                let n0 = max 0 (n - 1),
                n' <- choose (0, n0),
                t' <- elements [TBool, TLit],
                (nA, nB) <- elements [(n0, n'), (n', n0)]]

gVar :: (MonadMaybe m, MonadRand m) => CoAlgM m VarF TypedSeed
gVar (n, t, ts) = do let ts' = zip ts [0..]
                     let list = if t == Nothing then ts' else filter ((== t) . Just . fst) ts'
                     if null list then none else elements list >>= (return . Var . snd)

gLam :: (MonadMaybe m, MonadRand m) => CoAlgM m LamF TypedSeed
gLam (n, t, ts) = when (t `notElem` [Just TBool, Just TLit])
    [Lam t1 (n0, t2, t1:ts) |
        let n0 = max 0 (n - 1),
        (t1, t2) <- case t of Nothing              -> gType >>= \t1' -> return (t1', Nothing)
                              Just (TFunc t1' t2') -> return (t1', Just t2')]

gApp :: (MonadMaybe m, MonadRand m) => CoAlgM m AppF TypedSeed
gApp = liftS gApp'
  where gApp' (n, t) = when (preCond t) [App (nA, tA) (nB, tB) | 
            let n0 = max 0 (n - 1),
            n' <- choose (0, n0),
            (nA, nB) <- elements [(n0, n'), (n', n0)],
            (tA, tB) <- case t of Nothing -> [(Just (TFunc tp tr), Just tp) | tp <- gType, tr <- gType]
                                  Just tr -> [(Just (TFunc tp tr), Just tp) | tp <- gType]]
        arity :: Type -> Int
        arity (TFunc t1 t2) = 1 + arity t2
        arity _ = 0
        preCond :: Maybe Type -> Bool
        preCond (Just t) = arity t <= 2
        preCond _        = True

gType :: MonadRand m => m Type
gType = [t | t1 <- elements [TBool, TLit],
             t2 <- elements [TBool, TLit],
             t  <- elements [TBool, TLit, TFunc t1 t2]]

--Weighted.
coalgsWeighted :: CoAlgM Weighted LNG TypedSeed
coalgsWeighted = gLit |**| gAdd |**| gMul |**| gBool |**| gIf |**| gEqual |**| gVar |**| gLam |**| gApp

r2 :: Size -> IO (Fix LNG) -- r2: easily get infinite. Can hardly work.
r2 n = generateW [10,1,1,10,1,1,10,1,1] (n, Nothing, []) coalgsWeighted

--Dynamic. Works better.
instance Derive TypedSeed Int where derive (n, _, _) = n

class (Functor f, Traversable f) => RandomD f a where genD :: CoAlgM Dynamic f a
instance (RandomD f a, RandomD g a, Cardinality g, Arity f, Arity g, Derive a Int) => RandomD (f :+: g) a
  where genD = genD |***| genD
instance RandomD LitF   TypedSeed where genD = gLit
instance RandomD AddF   TypedSeed where genD = gAdd
instance RandomD MulF   TypedSeed where genD = gMul
instance RandomD BoolF  TypedSeed where genD = gBool
instance RandomD IfF    TypedSeed where genD = gIf
instance RandomD EqualF TypedSeed where genD = gEqual
instance RandomD VarF   TypedSeed where genD = gVar
instance RandomD LamF   TypedSeed where genD = gLam
instance RandomD AppF   TypedSeed where genD = gApp

coalgsDynamic :: CoAlgM Dynamic LNG TypedSeed
coalgsDynamic = gLit |***| gAdd |***| gMul |***| gBool |***| gIf |***| gEqual |***| gVar |***| gLam |***| gApp

r3 :: Size -> IO (Maybe (Fix LNG)) -- r3: easily get infinite when n is small. Now we set the bound.
r3 n = generateD 10000 (n, Nothing, []) coalgsDynamic

r3MayFail :: Size -> IO (Fix LNG)
r3MayFail n = fmap fromJust $ generateD 100000 (n, Nothing, []) coalgsDynamic
  -- putStrLn "************************"
  -- print e
  -- putStrLn "************************"
  -- printBFS e
  -- putStrLn "************************"
  -- print $ getDepth e
  -- putStrLn "************************"

-- Non-modular QuickCheck generator for Dynamic generating well-typed expressions.

type Input = (Int, Maybe N.Type, [N.Type])
data MaybeExpr = MaybeExpr { unMaybe :: Maybe (N.Expr) }

data M a = M { unM :: MaybeT (StateT Int (ReaderT Int G.Gen)) a } deriving Functor

instance Monad M where
  return = M . return
  (M m) >>= f = M $ m >>= unM . f

instance Applicative M where
  pure = M . pure
  (M f) <*> (M a) = M $ f <*> a

getMaxDepth :: M Int
getMaxDepth = M ask

getCounter :: M Int
getCounter = M get

update :: M ()
update = M $ modify pred

mChoose :: Random a => (a, a) -> M a
mChoose = M . lift . lift . lift . G.choose

mElements :: [a] -> M a
mElements = M . lift . lift . lift . G.elements

mToMaybe :: Int -> Int -> M N.Expr -> G.Gen MaybeExpr
mToMaybe bound maxD = fmap MaybeExpr . flip runReaderT maxD .  flip evalStateT bound . runMaybeT . unM

x :: Int -> IO (Maybe String)
x n = fmap (fmap show . unMaybe) $ G.generate (G.resize n Q.arbitrary)

instance Q.Arbitrary MaybeExpr where
  arbitrary = G.getSize >>= \n -> mToMaybe 10000 n $ gen (n, Nothing, [])
    where gen :: Input -> M N.Expr
          gen s@(_,t,_) = do
            c <- getCounter
            if c <= 0 then error "dd" else case t of
              Nothing      -> select s [wLit, wAdd, wMul, wBool, wIf, wEqual, wVar, wLam, wApp]
              Just N.TLit  -> select s [wLit, wAdd, wMul, wIf, wVar, wApp]
              Just N.TBool -> select s [wBool, wIf, wEqual, wVar, wApp]
              Just t0      -> if arity t0 <= 2 then select s [wIf, wVar, wLam, wApp]
                                               else select s [wIf, wVar, wLam]
          getWeight a n m = prob 3 a (fromIntegral (1 + n) / fromIntegral (2 + m))
          toWeighted arity f s@(n, _, _) = f s >>= \a -> getMaxDepth >>= \m -> return (getWeight arity n m, a)
          select :: Input -> [(Int, Input -> M N.Expr)] -> M N.Expr
          select input@(n,_,_) gens = do
            maxD <- getMaxDepth
            let weights = map (\x -> getWeight (fst x) n maxD) gens
            rs <- permutation weights 
            update >> (foldl1 (\(M x) (M y) -> M (x `mplus` y)) $ fmap (\i -> snd (gens !! i) input) rs)
            where permutation :: [Double] -> M [Int]
                  permutation xs = iterate (zip xs [0..])
                  iterate :: [(Double, a)] -> M [a]
                  iterate [(_, x)] = return [x]
                  iterate xs = do
                    (i, r) <- frequencyList xs
                    rs <- iterate $ take i xs ++ drop (i + 1) xs
                    return (r:rs)
                  frequencyList :: [(Double, a)] -> M (Int, a)
                  frequencyList xs0 = mChoose (0, tot) >>= (\r -> return $ pick r xs0 0)
                    where tot = sum (map fst xs0)
                          pick n ((k,x):xs) i
                            | n <= k    = (i, x)
                            | otherwise = pick (n-k) xs (i+1)
          wLit   = (0, genLit)
          wAdd   = (2, genAdd)
          wMul   = (2, genMul)
          wBool  = (0, genBool)
          wIf    = (3, genIf)
          wEqual = (2, genEqual)
          wVar   = (0, genVar)
          wLam   = (1, genLam)
          wApp   = (2, genApp)
          arity :: N.Type -> Int
          arity (N.TFunc t1 t2) = 1 + arity t2
          arity  _              = 0
          genType = [t | t1 <- mElements [N.TBool, N.TLit],
                         t2 <- mElements [N.TBool, N.TLit],
                         t  <- mElements [N.TBool, N.TLit, N.TFunc t1 t2]]
          genLit  (n, t, ts) = [N.Lit x | x <- mChoose (0, 100)]
          genBool (n, t, ts) = [N.BoolV x | x <- mChoose (False, True)]
          genVar  (n, t, ts) = let ts' = zip ts [0..] in
                               let list = if t == Nothing then ts' else filter ((== t) . Just . fst) ts' in
                               if null list then M none else[N.Var x | (_, x) <- mElements list]
          genAdd (n, t, ts) = do
            let n0 = max 0 (n - 1)
            n' <- mChoose (0, n0)
            (nA, nB) <- mElements [(n0, n'), (n', n0)]
            eA <- gen (nA, Just N.TLit, ts)
            eB <- gen (nB, Just N.TLit, ts)
            return $ N.Add eA eB
          genMul (n, t, ts) = do
            let n0 = max 0 (n - 1)
            n' <- mChoose (0, n0)
            (nA, nB) <- mElements [(n0, n'), (n', n0)]
            eA <- gen (nA, Just N.TLit, ts)
            eB <- gen (nB, Just N.TLit, ts)
            return $ N.Mul eA eB
          genIf (n, t, ts) = do
            let n0 = max 0 (n - 1)
            n1 <- mChoose (0, n0)
            n2 <- mChoose (0, n0)
            t' <- case t of Just t0 -> return t0
                            Nothing -> genType
            (nA, nB, nC) <- mElements [(n0, n1, n2), (n1, n0, n2), (n1, n2, n0)]
            eA <- gen (nA, Just N.TBool, ts)
            eB <- gen (nB, Just t', ts)
            eC <- gen (nC, Just t', ts)
            return $ N.If eA eB eC
          genEqual (n, t, ts) = do
            let n0 = max 0 (n - 1)
            n' <- mChoose (0, n0)
            (nA, nB) <- mElements [(n0, n'), (n', n0)]
            t' <- mElements [N.TBool, N.TLit]
            eA <- gen (nA, Just t', ts)
            eB <- gen (nB, Just t', ts)
            return $ N.Equal eA eB
          genLam (n, t, ts) = do
            let n0 = max 0 (n - 1)
            (t1, t2) <- case t of Nothing                -> genType >>= \t1' -> return (t1', Nothing)
                                  Just (N.TFunc t1' t2') -> return (t1', Just t2')
            e <- gen (n0, t2, t1:ts)
            return $ N.Lam t1 e
          genApp (n, t, ts) = do
            let n0 = max 0 (n - 1)
            n' <- mChoose (0, n0)
            (nA, nB) <- mElements [(n0, n'), (n', n0)]
            (tA, tB) <- case t of Nothing -> [(Just (N.TFunc tp tr), Just tp) | tp <- genType, tr <- genType]
                                  Just tr -> [(Just (N.TFunc tp tr), Just tp) | tp <- genType]
            eA <- gen (nA, tA, ts)
            eB <- gen (nB, tB, ts)
            return $ N.App eA eB
