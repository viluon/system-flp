module Eval (
  eval
) where

import qualified Data.Map as M
import Data.Either

import Syntax.AST
import HasType
import Utils

evalInfer :: Env -> TermInfer -> Value
evalInfer env (TermAnn     e    _)    = evalCheck env e
evalInfer _   (TermNat     n)         = ValNat n
evalInfer _   (TermFree    name)      = ValFree name
evalInfer _   (TermBuiltin b)         = ValBuiltin b
evalInfer env (TermBound   n    name) = case env !!? n of
                                        Just x  -> x
                                        Nothing -> error $ "undefined reference to " ++ name
evalInfer env (TermApp     e1   e2)   = valueApp (evalInfer env e1) (evalCheck env e2)
evalInfer env (TermTyLam   name e)    = ValTyLam name e env
evalInfer env (TermTyApp   e    tp)   = typeApp (evalInfer env e) tp

eval :: Env -> TermCheck -> Value
eval = evalCheck

evalCheck :: Env -> TermCheck -> Value
evalCheck env (TermCheckInf t)      = evalInfer env t
evalCheck env (TermCheckLam name e) = ValLam name e env

valueApp :: Value -> Value -> Value
valueApp (ValLam _ body env)     arg  = evalCheck (arg : env) body
valueApp pap@(ValPap a _ _)      arg1 = case a of
                                         1 -> uncurry builtinApp $ collect (ValPap 0 pap arg1)
                                         _ -> ValPap (a - 1) pap arg1
valueApp v@(ValBuiltin builtin) arg  = case arity $ fromRight undefined $ typeOf builtin of
                                         1 -> builtinApp builtin [arg]
                                         a -> ValPap (a - 1) v arg
valueApp a b                           = ValApp a b

collect :: Value -> (Builtin, [Value])
collect pap@(ValPap 0 _ _) = go pap []
  where go (ValPap _ (ValBuiltin b) x) xs = (b, x:xs)
        go (ValPap _ inner          x) xs = go inner (x:xs)
        go _                           _  = error "bug in collect"
collect _                   = error "collect only works with partial applications"

builtinApp :: Builtin -> [Value] -> Value
builtinApp builtin args = case arity $ fromRight undefined $ typeOf builtin of -- FIXME: ugliness with undefined
  2 -> let [a, b] = args in (binOps M.! builtin) (a, b)
  1 -> let [a]    = args in (unOps  M.! builtin)  a
  _ -> undefined

typeApp :: Value -> Type -> Value
typeApp (ValTyLam _ body env) _ = evalCheck env (TermCheckInf body)
typeApp a b                      = ValTyApp a b


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


