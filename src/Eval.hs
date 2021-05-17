{-# OPTIONS_GHC -Wall #-}

module Eval (
  eval
) where

import Syntax.AST
import qualified Value as Val

eval :: Val.Env -> Term -> Val.Value
eval env (TermAnn     e    _)    = eval env e
eval _   (TermFree    name)      = Val.Free name
eval env (TermBound   n    name) = env !! n
eval env (TermApp     e1   e2)   = valueApp (eval env e1) (eval env e2)
eval env (TermLam     name e)    = Val.Lam name e env
eval env (TermTyLam   name e)    = undefined
eval env (TermTyApp   e    tp)   = undefined

valueApp :: Val.Value -> Val.Value -> Val.Value
valueApp (Val.Lam _ body env) arg = eval (arg : env) body
valueApp a b                      = Val.App a b


foo :: Val.Value
foo = eval [] (TermApp (TermLam "x" (TermBound 0 "x")) (TermFree "foo"))

-- >>> foo
-- Free "foo"

-- λ x. (* (+ 2 3) x)
-- -> <lambda>

-- (λ s z. s (s (s z)))
-- chceš v LC -> (λ s z. s (s (s z)))
-- realita    -> <function@da344feb>

{-
var x = 0;

function foo() {
  x = x + 1;
}

x = 4;

foo();
x == 4;
-}

-- eval "f x"
-- f x

-- ts[τ]
-- iff ts `hasKind` * -> ?


-- 3 + x :: Int
-- -> x :: Int

-- (λ x. x :: Int) :: Int -> Int


