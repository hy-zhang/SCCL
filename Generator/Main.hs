{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures #-}

module Main where

import Base
import AST
import Criterion.Main
import qualified AST_NonModular as N
import Test.QuickCheck
import Test.QuickCheck.Gen
import GenExpr_ConstrSize
import qualified GenExpr_Typed_DepthSize as T
import qualified Base_MRM_FutureWork as M
import AST_Print
import Criterion.Types
import qualified GenExpr_ConstrSize as C

-- The function we're benchmarking.
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

quickC :: Int -> IO String
quickC n = fmap pprint $ generate (resize n arbitrary)
  where pprint :: N.Expr -> String
        pprint = show

quickC2 :: Int -> IO (Maybe String)
quickC2 n = fmap pprint $ generate (resize n arbitrary)
  where pprint :: T.MaybeExpr -> Maybe String
        pprint = fmap show . T.unMaybe

myConfig = defaultConfig { timeLimit = 20 }
-- Our benchmark harness.
main :: IO ()
main = do
  defaultMainWith myConfig [
    bgroup "Mean Exec. Time in ms" [
                         -- bench "E6_10" $ nfIO $ fmap (fmap pretty) $ T.r3 10
                       -- , bench "QC_10" $ nfIO $ quickC2 10
                       -- , bench "E6_15" $ nfIO $ fmap (fmap pretty) $ T.r3 15
                       -- , bench "QC_15" $ nfIO $ quickC2 15
                       -- , bench "E6_20" $ nfIO $ fmap (fmap pretty) $ T.r3 20
                       -- , bench "QC_20" $ nfIO $ quickC2 20
                       -- , bench "E6_25" $ nfIO $ fmap (fmap pretty) $ T.r3 25
                       -- , bench "QC_25" $ nfIO $ quickC2 25
                       -- , bench "E6_30" $ nfIO $ fmap (fmap pretty) $ T.r3 30
                       -- , bench "QC_30" $ nfIO $ quickC2 30
                            bench "E1_20" $ nfIO $ fmap pretty $ C.r2' 20
                          , bench "E1_40" $ nfIO $ fmap pretty $ C.r2' 40
                          , bench "E1_60" $ nfIO $ fmap pretty $ C.r2' 60
                          , bench "E1_80" $ nfIO $ fmap pretty $ C.r2' 80
                          , bench "E1_100" $ nfIO $ fmap pretty $ C.r2' 100
                       ]
    ]
