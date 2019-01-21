{-# LANGUAGE TypeOperators, DeriveFunctor, StandaloneDeriving,
 FlexibleContexts, UndecidableInstances, MultiParamTypeClasses,
 FlexibleInstances, DeriveFoldable, DeriveTraversable, RankNTypes,
 ScopedTypeVariables, AllowAmbiguousTypes, KindSignatures,
 TypeFamilies, DataKinds, AutoDeriveTypeable, FunctionalDependencies,
 PatternSynonyms, ViewPatterns #-}

import Base
import EvalMonad
import EvalCombinators
import Control.Monad
import Data.Functor.Identity
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Except

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

z :: ArithF :<: f => Fix f
z = In . inj $ TmZero

s :: ArithF :<: f => Fix f -> Fix f
s = In . inj . TmSucc

p :: ArithF :<: f => Fix f -> Fix f
p = In . inj . TmPred

isZ :: ArithF :<: f => Fix f -> Fix f
isZ = In . inj . TmIsZero

true :: BoolF :<: f => Fix f
true = In . inj $ TmTrue

false :: BoolF :<: f => Fix f
false = In . inj $ TmFalse

ifC :: BoolF :<: f => Fix f -> Fix f -> Fix f -> Fix f
ifC x y z = In . inj $ TmIf x y z

e :: Fix (ArithF :+: BoolF)
e = ifC false zero . iszero . pred $ succ zero

-- Patterns

pattern Zero        <- (proj . out -> Just TmZero)
pattern Succ e      <- (proj . out -> Just (TmSucc e))
pattern Pred e      <- (proj . out -> Just (TmPred e))
pattern IsZero e    <- (proj . out -> Just (TmIsZero e))
pattern If e1 e2 e3 <- (proj . out -> Just (TmIf e1 e2 e3))
pattern T           <- (proj . out -> Just TmTrue)
pattern F           <- (proj . out -> Just TmFalse)

-- Auxiliary Functions

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

-- Coalgebras

evalArith :: (ArithF :<: f, MonadEval m, IsNumericVal f) => CoAlgP f m ArithF (Fix f)
evalArith (Pred Zero)               = rdcRule . Right $ TmZero
evalArith (Pred (Succ e)) | isNum e = rdcRule . Left $ e
evalArith (Pred e)                  = cgrRule . Right $ TmPred (Right e)
evalArith (Succ e)                  = cgrRule . Right $ TmSucc (Right e)
evalArith (IsZero e)                = cgrRule . Right $ TmIsZero (Right e)
evalArith _                         = noMatch

evalBool :: (ArithF :<: f, BoolF :<: f, MonadEval m, IsNumericVal f) => CoAlgP f m BoolF (Fix f)
evalBool (IsZero Zero)               = rdcRule . Right $ TmTrue
evalBool (IsZero (Succ e)) | isNum e = rdcRule . Right $ TmFalse
evalBool (If T x y)                  = rdcRule . Left $ x
evalBool (If F x y)                  = rdcRule . Left $ y
evalBool (If x y z)                  = cgrRule . Right $ TmIf (Right x) (Left y) (Left z)
evalBool _                           = noMatch

-- Testing

r :: Fix (ArithF :+: BoolF) -> Either EvalError (Fix (ArithF :+: BoolF))
r = runIdentity . runExceptT . flip evalStateT (False, False) . unEvalT . unfoldP (unify $ evalArith |*| evalBool)

run :: Fix (ArithF :+: BoolF) -> IO ()
run x = case r x of
  Left e -> fail (show e)
  Right e -> do
    putStrLn $ "==> " ++ show x
    if (show x /= show e) then run e else putStrLn "DONE"

x :: Fix (ArithF :+: BoolF)
x = ifC false z (isz $ p $ s $ z)
