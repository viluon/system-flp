module Typechecking.Context (
  Context
, Info(..)
) where

import Syntax.AST

data Info
  = HasKind Kind
  | HasType Type
  deriving (Eq, Show, Ord)

type Context = [(Name, Info)]
