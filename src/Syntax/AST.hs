module Syntax.AST (
  Type(..)
, TermInfer(..)
, TermCheck(..)
, Kind(..)
, Name
) where

type Name = String

data Kind = Star
          deriving (Eq, Show, Ord)

data Type
  = TyFree   Name      -- of kind *
  | TyNat              -- of kind *
  | TyFun    Type Type -- of kind *
  | TyForall Name Type -- of kind * -> *
  deriving (Eq, Show, Ord)

data TermInfer
  = TermAnn   TermCheck Type      -- type annotation (x :: τ)
  | TermNat   Int                 -- natural number literal (42)
  | TermFree  Name                -- unbound (global) variable
  | TermBound Int       Name      -- bound variable
  | TermApp   TermInfer TermCheck -- application (f x)
  | TermTyLam Name      TermInfer -- type lambda (Λ τ. e)
  | TermTyApp TermInfer Type      -- type application (f[τ])
  deriving (Eq, Show, Ord)

data TermCheck
  = TermCheckLam Name TermCheck   -- regular lambda (λ x. e)
  | TermCheckInf TermInfer        -- typecheck an inferrable term
  deriving (Eq, Show, Ord)

