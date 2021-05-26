module Syntax.AST
( Type(..)
, TermInfer(..)
, TermCheck(..)
, Kind(..)
, Name
, Builtin(..)
, Value(..)
, Env
, builtinOps
, binOps
, unOps
, arity
) where

import qualified Data.Map as M

type Env = [Value]
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
  = TermAnn     TermCheck Type      -- type annotation (x :: τ)
  | TermNat     Int                 -- natural number literal (42)
  | TermFree    Name                -- unbound (global) variable
  | TermBuiltin Builtin             -- builtin operation (plus)
  | TermBound   Int       Name      -- bound variable
  | TermApp     TermInfer TermCheck -- application (f x)
  | TermTyLam   Name      TermInfer -- type lambda (Λ τ. e)
  | TermTyApp   TermInfer Type      -- type application (f[τ])
  deriving (Eq, Show, Ord)

data TermCheck
  = TermCheckLam Name TermCheck   -- regular lambda (λ x. e)
  | TermCheckInf TermInfer        -- typecheck an inferrable term
  deriving (Eq, Show, Ord)

data Value
  = ValFree    Name
  | ValNat     Int
  | ValLam     Name TermCheck Env
  | ValBuiltin Builtin
  | ValApp     Value Value
  | ValPap     Int Value Value -- ^ arity (arity of λ - 1), lambda, argument
  | ValTyLam   Name TermInfer Env
  | ValTyApp   Value Type
  deriving (Eq, Show)

data Builtin
  = Plus
  | Times
  | Negate
  deriving (Eq, Show, Ord)

builtinOps :: M.Map Name Value
builtinOps = M.fromList
             [ ("plus",   ValBuiltin Plus)
             , ("krát",   ValBuiltin Times)
             , ("times",  ValBuiltin Times)
             , ("negate", ValBuiltin Negate)
             ]

binOps :: M.Map Builtin ((Value, Value) -> Value)
binOps = M.fromList
         [ (Plus,  \(ValNat x, ValNat y) -> ValNat $ x + y)
         , (Times, \(ValNat x, ValNat y) -> ValNat $ x * y)
         ]
unOps  :: M.Map Builtin (Value          -> Value)
unOps  = M.fromList [(Negate, \(ValNat n) -> ValNat $ negate n)]

arity :: Builtin -> Int
arity Plus   = 2
arity Times  = 2
arity Negate = 1
