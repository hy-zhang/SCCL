{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 PatternSynonyms, ViewPatterns #-}

module EvalCombinators where

import Base
import EvalMonad
import Data.Functor.Identity
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Except

-- Apomorphism

type CoAlgP g m f a = a -> m (Either (Fix g) (f (Either (Fix g) a)))

unfoldP :: (Functor f, Traversable f, Monad m) => CoAlgP f m f a -> a -> m (Fix f)
unfoldP f = join . fmap (h . fmap (fmap In . sequence . fmap (h . fmap (unfoldP f)))) . f
  where h :: Monad m => Either (Fix f) (m (Fix f)) -> m (Fix f)
        h (Left x) = return x
        h (Right x) = x

-- The Monad

type E = EvalT Identity

infixr |*|
(|*|) :: CoAlgP h E f a -> CoAlgP h E g a -> CoAlgP h E (f :+: g) a
cF |*| cG = \a -> fmap (fmap Inl) (cF a) `compose` fmap (fmap Inr) (cG a)

unify :: CoAlgP f E g (Fix f) -> CoAlgP f E g (Fix f)
unify coalg x = ifDone returnX (coalg x `catchNoMatch` returnX)
  where returnX = return $ Left x

single_step :: Show (Fix f) => (Fix f -> E (Fix f)) -> Fix f -> IO ()
single_step f e = do
  pretty e
  case evalE (f e) of
    Left error -> print error
    Right e'   -> putStrLn $ "=> " ++ show e'

multi_step :: (Show (Fix f), Eq (Fix f)) => (Fix f -> E (Fix f)) -> Fix f -> IO ()
multi_step f e0 = do
  pretty e0
  repeat e0
  where repeat e = case evalE (f e) of
          Left error -> print error
          Right e'   -> if e == e' then return ()
                        else pretty e' >> repeat e'

evalE :: E (Fix f) -> Either EvalError (Fix f)
evalE = runIdentity . runExceptT . flip evalStateT (False, True) . unEvalT

pretty :: Show (Fix f) => Fix f -> IO ()
pretty e = putStrLn ("=> " ++ show e)
  