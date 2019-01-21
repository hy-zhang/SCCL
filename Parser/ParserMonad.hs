{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies #-}

module ParserMonad where

import MaybeMonad
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Data.Char
import Data.Maybe

-- Library of (Monadic) Parsing

-- 1. Definition of Monad Transformer
newtype ParserT m a = ParserT { unParserT :: MaybeT (StateT String m) a } deriving Functor

-- 2. Instances of Functor, Applicative and Monad
instance Monad m => Monad (ParserT m) where
  return = ParserT . return
  (ParserT m) >>= f = ParserT $ m >>= unParserT . f

instance (Functor f, Monad f) => Applicative (ParserT f) where
  pure = ParserT . pure
  (ParserT f) <*> (ParserT a) = ParserT $ f <*> a

-- 3. Type Class for Monad Transformer
class Monad m => MonadParser m where
  choice :: m a -> m a -> m a
  failP :: m a
  fetch :: m Char
  sat :: (Char -> Bool) -> m Char
  sat p = fetch >>= \x -> if p x then return x else failP
  char :: Char -> m Char
  char c = sat (== c)
  string :: String -> m String
  string ""     = return ""
  string (c:cs) = char c >> string cs >> return (c:cs)
  many :: m a -> m [a]
  many p = many1 p <||> return [] 
  many1 :: m a -> m [a]
  many1 p = p >>= \x -> many p >>= \xs -> return (x:xs)
  int :: m Int
  int = fmap read . many1 $ sat isDigit

-- 4. Instances of Monad Transformer class 
instance Monad m => MonadParser (ParserT m) where
  fetch = ParserT $ get >>= \s -> if empty s then none
    else put (tail s) >> return (head s)
  failP = ParserT none
  choice x y = ParserT $ MaybeT $ do
    s <- get
    resX <- runMaybeT (unParserT x)
    case resX of
      Just _  -> return resX
      Nothing -> put s >> runMaybeT (unParserT y)
