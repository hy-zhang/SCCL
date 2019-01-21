{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures #-}

module AST_BFS where

import AST
import Base
import qualified Data.Map.Strict as M

class BFSAlg (f :: * -> *) where bfsAlg :: Alg f (Int -> [(String, [Int])])
instance (BFSAlg f, BFSAlg g) => BFSAlg (f :+: g) where
  bfsAlg (Inl x) = bfsAlg x
  bfsAlg (Inr x) = bfsAlg x

instance BFSAlg LitF   where bfsAlg (Lit x)     i = [("Lit", [i])]
instance BFSAlg AddF   where bfsAlg (Add x y)   i = ("Add", [i]) : x (succ i) ++ y (succ i)
instance BFSAlg MulF   where bfsAlg (Mul x y)   i = ("Mul", [i]) : x (succ i) ++ y (succ i)
instance BFSAlg BoolF  where bfsAlg (BoolV x)   i = [("BoolV", [i])]
instance BFSAlg IfF    where bfsAlg (If x y z)  i = ("If", [i]) : x (succ i) ++ y (succ i) ++ z (succ i)
instance BFSAlg EqualF where bfsAlg (Equal x y) i = ("Equal", [i]) : x (succ i) ++ y (succ i)
instance BFSAlg VarF   where bfsAlg (Var x)     i = [("Var", [i])]
instance BFSAlg LamF   where bfsAlg (Lam t x)   i = ("Lam", [i]) : x (succ i)
instance BFSAlg AppF   where bfsAlg (App x y)   i = ("App", [i]) : x (succ i) ++ y (succ i)

bfs :: Fix LNG -> M.Map String [Int]
bfs x = foldr (\(k, v) -> M.insertWith (++) k v) M.empty $ fold bfsAlg x 0

printBFS :: Fix LNG -> String
printBFS = M.foldrWithKey f "" . bfs
  where f k list res = res ++ k ++ ": " ++ show (length list) ++ " " ++ show (mean list) ++ "\n"
        mean :: [Int] -> Double
        mean list = fromIntegral (sum list) / fromIntegral (length list)