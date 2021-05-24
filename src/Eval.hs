module Eval (
  evalInfer
, evalCheck
) where

import Syntax.AST
import qualified Value as Val

(!!?) :: [a] -> Int -> Maybe a
xs !!? n | n >= 0 && n < length xs = Just $ xs !! n
_  !!? _ = Nothing

evalInfer :: Val.Env -> TermInfer -> Val.Value
evalInfer env (TermAnn     e    _)    = evalCheck env e
evalInfer _   (TermNat     n)         = Val.Nat n
evalInfer _   (TermFree    name)      = Val.Free name
evalInfer env (TermBound   n    name) = case env !!? n of
                                        Just x  -> x
                                        Nothing -> error $ "undefined reference to " ++ name
evalInfer env (TermApp     e1   e2)   = valueApp (evalInfer env e1) (evalCheck env e2)
evalInfer env (TermTyLam   name e)    = Val.TyLam name e env
evalInfer env (TermTyApp   e    tp)   = typeApp (evalInfer env e) tp

evalCheck :: Val.Env -> TermCheck -> Val.Value
evalCheck env (TermCheckInf t)      = evalInfer env t
evalCheck env (TermCheckLam name e) = Val.Lam name e env

valueApp :: Val.Value -> Val.Value -> Val.Value
valueApp (Val.Lam _ body env) arg = evalCheck (arg : env) body
valueApp a b                      = Val.App a b

typeApp :: Val.Value -> Type -> Val.Value
typeApp (Val.TyLam _ body env) arg = evalCheck env (TermCheckInf body)
typeApp a b                        = Val.TyApp a b


{-
(τ -> τ) -> (τ -> τ)

(λ x. x) :: forall τ. τ -> τ

(λ y. y :: τ -> τ) (f :: τ)
--> type error: τ ≠ (τ -> τ)
-}

-- >>> bar
-- Free "f"

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


