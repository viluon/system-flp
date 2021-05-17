module Value (
  Value(..)
, Env
) where

import Syntax.AST

type Env = [Value]

data Value
  = Free  Name
  | Lam   Name Term Env
  | App   Value Value
  | TyLam Name Term Env
  | TyApp Value Value
  deriving (Eq, Show)
