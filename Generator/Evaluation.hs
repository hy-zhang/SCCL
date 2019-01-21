{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 GADTs, MonadComprehensions, PatternSynonyms, ViewPatterns #-}

module Evaluation where

import Base
import Base_MonadTrans
import Base_RandMonad
import Base_GenCombinators

import AST
import AST_Print
import AST_BFS
import AST_Depth
import AST_TypeCheck
import qualified AST_NonModular as N

import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as G
import qualified Data.Map.Strict as M

import qualified GenExpr_ConstrSize as EC
import qualified GenExpr_DepthSize as ED
import qualified GenExpr_Typed_DepthSize as ET

evaluator :: Int -> Int -> (Int -> IO (Fix LNG)) -> IO ()
evaluator size iterate gen = do
  es <- sequence $ replicate iterate $ gen size
  let maps = map (countInMap . bfs) es
  let m = foldr1 combineMap maps
  putStrLn $ printMap m
  let depths = sum $ map getDepth es
  putStrLn $ "AVG.DEPTH = " ++ show (divD depths iterate)
  let typed = sum $ map (\e -> if typeCheck e == Nothing then 0 else 1) es
  let sizes = sum $ map getSize es
  putStrLn $ "AVG.Size(total) = " ++ show (divD sizes iterate)
  -- putStrLn $ "#typed = " ++ show typed
  where mean :: [Int] -> Double
        mean list = divD (sum list) (length list)
        countInMap :: M.Map String [Int] -> M.Map String (Int, Double, Int)
        countInMap = M.map $ \list -> (length list, mean list, 1)
        combineMap :: M.Map String (Int, Double, Int) -> M.Map String (Int, Double, Int) -> M.Map String (Int, Double, Int)
        combineMap = M.unionWith $ \(x1, y1, z1) (x2, y2, z2) -> (x1 + x2, y1 + y2, z1 + z2)
        printMap = M.foldrWithKey (\k (v1, v2, v3) acc -> acc ++ k ++ ": avg.count = " ++
          show (divD v1 iterate) ++ " avg.depth = " ++ show (v2 / (fromIntegral v3)) ++ "\n") ""
        divD :: Int -> Int -> Double
        divD a b = fromIntegral a / fromIntegral b

e1 size it = evaluator size it EC.r2
e2 size it = evaluator size it EC.r3MayFail
e3 size it = evaluator size it ED.r2
e4 size it = evaluator size it ED.r3MayFail
e5 size it = evaluator size it ET.r2
e6 size it = evaluator size it ET.r3MayFail