{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures #-}

module GenExpr_Enum_DepthSize where

import Base
import AST
import AST_TypeCheck
import Data.List (nub)
import Test.QuickCheck (Gen, generate, arbitrary)
import System.Random (newStdGen)
import Data.Maybe (isJust)

type Size = Int
type MaxId = Int
type IntSet = [Int]
type Seed = (Size, MaxId, IntSet)

when :: Bool -> [a] -> [a]
when True xs = xs
when _    _  = []

liftS1 :: (Functor f, Functor m) => CoAlgM m f (Size, IntSet) -> CoAlgM m f Seed
liftS1 c (size, max, set) = fmap (fmap (\(x, y) -> (x, max, y))) (c (size, set))

liftS2 :: (Functor f, Functor m) => CoAlgM m f (Size, MaxId) -> CoAlgM m f Seed
liftS2 c (size, max, set) = fmap (fmap (\(x, y) -> (x, y, set))) (c (size, max))

liftS3 :: (Functor f, Functor m) => CoAlgM m f Size -> CoAlgM m f Seed
liftS3 c (size, max, set) = fmap (fmap (\x -> (x, max, set))) (c size)

pairs n = (n - 1, n - 1) : concat [[(x, n - 1), (n - 1, x)] | x <- [1..n-2]]

triples n = (n - 1, n - 1, n - 1) : (singles n ++ doubles n)
  where singles n = concat [[(n - 1, x, y), (x, n - 1, y), (x, y, n - 1)] | x <- [0..n-2], y <- [0..n-2]]
        doubles n = concat [[(n - 1, n - 1, x), (n - 1, x, n - 1), (x, n - 1, n - 1)] | x <- [0..n-2]]

eLit :: CoAlgM [] LitF Seed
eLit = liftS1 eLit'
  where eLit' (n, xs) = when (n <= 0) $ map Lit xs

eAdd :: CoAlgM [] AddF Seed
eAdd = liftS3 eAdd'
  where eAdd' n = when (n > 0) $ [Add x y | (x, y) <- pairs n]

eBool :: CoAlgM [] BoolF Seed
eBool = liftS3 eBool'
  where eBool' n = when (n <= 0) $ [BoolV True, BoolV False]

eMul :: CoAlgM [] MulF Seed
eMul = liftS3 eMul'
  where eMul' n = when (n > 0) $ [Mul x y | (x, y) <- pairs n]

eIf :: CoAlgM [] IfF Seed
eIf = liftS3 eIf'
  where eIf' n = when (n > 0) $ [If x y z | (x, y, z) <- triples n]

eEqual :: CoAlgM [] EqualF Seed
eEqual = liftS3 eEqual'
  where eEqual' n = when (n > 0) $ [Equal x y | (x, y) <- pairs n]

eVar :: CoAlgM [] VarF Seed
eVar = liftS2 eVar'
  where eVar' (n, max) = when (n <= 0) $ map Var [0..max]

eLam :: CoAlgM [] LamF Seed
eLam = liftS2 eLam'
  where eLam' (n, max) = when (n > 0) $ [Lam t (n - 1, max + 1) | t <- eType]

eApp :: CoAlgM [] AppF Seed
eApp = liftS3 eApp'
  where eApp' n = when (n > 0) $ [App x y | (x, y) <- pairs n]

eType :: [Type]
eType = xs ++ [TFunc x y | x <- xs, y <- xs]
  where xs = [TBool, TLit]



-- run :: [Int] -> Int -> [Expr]
-- run xs n = enumerator initial
  -- where initial :: NumBoundSized = (n, xs, -1)

-- test :: [Int] -> Int -> IO ()
-- test xs n = do
  -- let es = run xs n
  -- let total = length es
  -- let typed :: Int = foldr (\x acc -> if isJust (typeCheck x) then acc + 1 else acc) 0 es 
  -- putStrLn $ "total = " ++ show total ++ " typed = " ++ show typed
  
-- Very inefficient.
-- Alternative: increasing n. Use a table.
  
-- Number of total(typed):
-- size = 1: (increasing xs)
-- 3(3), 4(4), 5(5), 6(6), 7(7)...
-- size = 2: (increasing xs)
-- 87(41), 158(62), 261(93), 402(134), 587(185)...
-- size = 3: (increasing xs)
-- 762285(5111), 4357942(14751)...