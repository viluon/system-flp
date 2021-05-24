module Value
( Builtin(..)
, Value(..)
, Env
, arity
, binOps
, unOps
) where

import qualified Data.Map as M
import Syntax.AST

type Env = [Value]

data Value
  = Free     Name
  | Nat      Int
  | Lam      Name TermCheck Env
  | VBuiltin Builtin
  | App      Value Value
  | Pap      Int Value Value -- ^ arity (arity of λ - 1), lambda, argument
  | TyLam    Name TermInfer Env
  | TyApp    Value Type
  deriving (Eq, Show)

-- takeThree : a -> b -> c -> d
-- takeThree a b :: c -> d ≃ Val.Pap 1 (Val.Pap 2 takeThree a) b
-- takeThree c ≃ Val.Pap 1 _ ---> do the application

data Builtin
  = Plus
  | Times
  | Negate
  deriving (Eq, Show, Ord)

binOps :: M.Map Builtin ((Value, Value) -> Value)
binOps = M.fromList
          [ (Plus,  \(Nat x, Nat y) -> Nat $ x + y)
          , (Times, \(Nat x, Nat y) -> Nat $ x * y)
          ]
unOps  :: M.Map Builtin (Value          -> Value)
unOps  = M.fromList [(Negate, \(Nat n) -> Nat $ negate n)]

arity :: Builtin -> Int
arity Plus   = 2
arity Times  = 2
arity Negate = 1

-- instance TypedVal Builtin where
--   typeOf Plus = TyFun ()

-- class TypedVal a where
--   typeOf :: a -> Type
