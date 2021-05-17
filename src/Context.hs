module Context (
) where

import Syntax.AST

data Info
  = HasKind Kind
  | HasType Type

type Context = [(Name, Type)]
