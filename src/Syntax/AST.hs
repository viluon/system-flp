module Syntax.AST (
  Type(..)
, Term(..)
, Name
, Kind
) where

type Name = String

data Kind

data Type
  = TyFree   Name      -- of kind *
  | TyFun    Type Type -- of kind *
  | TyForall Name Type -- of kind * -> *
  deriving (Eq, Show)

data Term
  = TermAnn   Term Type -- type annotation (x :: τ)
  | TermFree  Name      -- unbound (global) variable
  | TermBound Int  Name -- bound variable
  | TermApp   Term Term -- application (f x)
  | TermLam   Name Term -- regular lambda (λ x. e)
  | TermTyLam Name Term -- type lambda (Λ τ. e)
  | TermTyApp Term Type -- type application (f[τ])
  deriving (Eq, Show)

