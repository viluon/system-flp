module Value (
  Value(..)
, Env
) where

import Syntax.AST

type Env = [Value]

data Value
  = Free  Name
  | Nat   Int
  | Lam   Name TermCheck Env
  | App   Value Value
  | TyLam Name TermInfer Env
  | TyApp Value Type
  deriving (Eq, Show)
