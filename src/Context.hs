module Context (
  Context(..),
  Info(..)
) where

import Syntax.AST

data Info
  = HasKind Kind
  | HasType Type

type Context = [(Name, Info)]
