{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 PatternSynonyms, ViewPatterns #-}

module PriorMonad where

import Base
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Functor.Identity
import Control.Monad.Except
import Prelude hiding (pred, succ)
import Control.Monad.Trans

-- Library of Evaluation

-- 1. Definition of Error Type, and Monad Transformer
-- data EvalError = NoRulesApplied | ConflictReduction | ConflictCongruence deriving Show

-- type IsDone = Bool
-- type IsCongruenceRule = Bool
-- type EvalState = (IsDone, IsCongruenceRule)

newtype PriorT s e m a = PriorT {
  unPriorT :: MaybeT (StateT s (ExceptT e m)) a 
} deriving Functor

-- 2. Instances of Applicative and Monad
instance (Monad m, Ord s) => Monad (PriorT s e m) where
  return = PriorT . return
  (PriorT m) >>= f = PriorT $ m >>= unPriorT . f

instance (Functor f, Monad f, Ord s) => Applicative (PriorT s e f) where
  pure = return
  (<*>) = ap

-- 3. Type Class for Monad Transformer
class (Monad m, Ord s) => MonadPrior s e m | m -> s, m -> e where
  create    :: s -> a -> m a
  failure   :: m a
  priorComp :: (s -> e) -> m a -> m a -> m a
  catchFail :: m a -> m a -> m a

instance (Monad m, Ord s) => MonadPrior s e (PriorT s e m) where
  create s a = PriorT $ put s >> return a
  failure    = PriorT . MaybeT $ return Nothing
  priorComp f x y = PriorT $ MaybeT $ do
    s <- get  
    rX <- runMaybeT $ unPriorT x
    sX <- get
    put s
    rY <- runMaybeT $ unPriorT y
    sY <- get
    case (rX, rY, compare sX sY) of
      (Just _, Just _, GT) -> put sX >> return rX
      (Just _, Just _, LT) -> put sY >> return rY
      (Just _, Just _, EQ) -> throwError $ f sX
      (Just _, _     , _ ) -> put sX >> return rX
      (_     , Just _, _ ) -> put sY >> return rY
      _                    -> return Nothing
  catchFail x y = PriorT . MaybeT $ do
    r <- runMaybeT $ unPriorT x
    case r of Just _  -> return r
              Nothing -> runMaybeT $ unPriorT y

instance Ord s => MonadTrans (PriorT s e) where
  lift = PriorT . lift . lift . lift

instance MonadPrior s e m => MonadPrior s e (StateT s0 m) where
  create s a = lift $ create s a
  failure = lift failure
  priorComp f x y = get >>= \s -> lift $ priorComp f (evalStateT x s) (evalStateT y s)
  catchFail x y = get >>= \s -> lift $ catchFail (evalStateT x s) (evalStateT y s)

type Done = Bool
type IsRdc = Bool
data Conflict = ConflictRdc | ConflictCgr deriving Show

type Eval = StateT Done (PriorT IsRdc Conflict Identity)

type CoAlgP g m f a = a -> m (Either (Fix g) (f (Either (Fix g) a)))

(|<>|) :: CoAlgP h Eval f a -> CoAlgP h Eval g a -> CoAlgP h Eval (f :+: g) a
cF |<>| cG = \x -> priorComp report (fmap (fmap Inl) (cF x)) (fmap (fmap Inr) (cG x))
  where report :: Bool -> Conflict
        report True  = ConflictRdc
        report False = ConflictCgr

check :: CoAlgP f Eval f (Fix f) -> CoAlgP f Eval f (Fix f)
check coalg x = do
  isDone <- get
  if isDone then returnX
            else (put True >> coalg x) `catchFail` (put False >> returnX)
  where returnX = return (Left x)

unfoldP :: (Traversable f, Monad m) => CoAlgP f m f a -> a -> m (Fix f)
unfoldP f = join . fmap (h . fmap (fmap In . sequence . fmap (h . fmap (unfoldP f)))) . f
  where h :: Monad m => Either (Fix f) (m (Fix f)) -> m (Fix f)
        h (Left x) = return x
        h (Right x) = x

reduce :: Traversable f => Fix f -> CoAlgP f Eval f (Fix f) -> IO (Fix f)
reduce e coalg = case res of
  Left x          -> error (show x)
  Right (Just e') -> return e'
  Right _         -> return e
  where res = runIdentity . runExceptT . flip evalStateT False .
              runMaybeT . unPriorT . flip evalStateT False $ unfoldP (check coalg) e

-- unify :: CoAlgP f E g (Fix f) -> CoAlgP f E g (Fix f)
-- unify coalg x = ifDone returnX (coalg x `catchNoMatch` returnX)
  -- where returnX = return $ Left x
  
-- Functors

data ArithF e = TmZero | TmSucc e | TmPred e | TmIsZero e deriving (Functor, Foldable, Traversable, Eq)
instance Show e => Show (ArithF e) where
  show TmZero = "zero"
  show (TmSucc e) = "succ (" ++ show e ++ ")"
  show (TmPred e) = "pred (" ++ show e ++ ")"
  show (TmIsZero e) = "iszero (" ++ show e ++ ")"

data BoolF e = TmTrue | TmFalse | TmIf e e e deriving (Functor, Foldable, Traversable, Eq)
instance Show e => Show (BoolF e) where
  show TmTrue = "true"
  show TmFalse = "false"
  show (TmIf x y z) = "if (" ++ show x ++ ")" ++ " then (" ++ show y ++ ") else (" ++ show z ++ ")"

-- Smart Constructors

zero :: ArithF :<: f => Fix f
zero = In . inj $ TmZero

succ :: ArithF :<: f => Fix f -> Fix f
succ = In . inj . TmSucc

pred :: ArithF :<: f => Fix f -> Fix f
pred = In . inj . TmPred

iszero :: ArithF :<: f => Fix f -> Fix f
iszero = In . inj . TmIsZero

true :: BoolF :<: f => Fix f
true = In . inj $ TmTrue

false :: BoolF :<: f => Fix f
false = In . inj $ TmFalse

ifC :: BoolF :<: f => Fix f -> Fix f -> Fix f -> Fix f
ifC x y z = In . inj $ TmIf x y z

e :: Fix (ArithF :+: BoolF)
e = ifC false zero . iszero . pred $ succ zero

-- Semantics

pattern Zero        <- (proj . out -> Just TmZero)
pattern Succ e      <- (proj . out -> Just (TmSucc e))
pattern Pred e      <- (proj . out -> Just (TmPred e))
pattern IsZero e    <- (proj . out -> Just (TmIsZero e))
pattern If e1 e2 e3 <- (proj . out -> Just (TmIf e1 e2 e3))
pattern T           <- (proj . out -> Just TmTrue)
pattern F           <- (proj . out -> Just TmFalse)

class Functor f => IsNumericVal (f :: * -> *) where isNumAlg :: Alg f Bool
isNum :: IsNumericVal f => Fix f -> Bool
isNum = fold isNumAlg

instance IsNumericVal ArithF where
  isNumAlg TmZero        = True
  isNumAlg (TmSucc True) = True
  isNumAlg _             = False

instance IsNumericVal BoolF where
  isNumAlg _ = False

instance (IsNumericVal f, IsNumericVal g) => IsNumericVal (f :+: g) where
  isNumAlg (Inl x) = isNumAlg x
  isNumAlg (Inr x) = isNumAlg x

rdcRule :: MonadPrior IsRdc Conflict m => a -> m a
rdcRule = create True

cgrRule :: MonadPrior IsRdc Conflict m => a -> m a
cgrRule = create False

evalArith :: (ArithF :<: f, MonadPrior IsRdc Conflict m, IsNumericVal f) => CoAlgP f m ArithF (Fix f)
evalArith (Pred Zero)               = rdcRule . Right $ TmZero
evalArith (Pred (Succ e)) | isNum e = rdcRule . Left $ e
evalArith (Pred e)                  = cgrRule . Right $ TmPred (Right e)
evalArith (Succ e)                  = cgrRule . Right $ TmSucc (Right e)
evalArith (IsZero e)                = cgrRule . Right $ TmIsZero (Right e)
evalArith _                         = failure

evalBool :: (ArithF :<: f, BoolF :<: f, MonadPrior IsRdc Conflict m, IsNumericVal f) => CoAlgP f m BoolF (Fix f)
evalBool (IsZero Zero)               = rdcRule . Right $ TmTrue
evalBool (IsZero (Succ e)) | isNum e = rdcRule . Right $ TmFalse
evalBool (If T x y)                  = rdcRule . Left $ x
evalBool (If F x y)                  = rdcRule . Left $ y
evalBool (If x y z)                  = cgrRule . Right $ TmIf (Right x) (Left y) (Left z)
evalBool _                           = failure
