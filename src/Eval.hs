module Eval (
  evalInfer
, evalCheck
) where

import Syntax.AST
import qualified Value as Val
import qualified Data.Map as M

(!!?) :: [a] -> Int -> Maybe a
xs !!? n | n >= 0 && n < length xs = Just $ xs !! n
_  !!? _ = Nothing

builtins :: M.Map Name Val.Value
builtins = M.fromList
         [ ("plus",   Val.VBuiltin Val.Plus)
         , ("krát",   Val.VBuiltin Val.Times)
         , ("times",  Val.VBuiltin Val.Times)
         , ("negate", Val.VBuiltin Val.Negate)
         ]

evalInfer :: Val.Env -> TermInfer -> Val.Value
evalInfer env (TermAnn     e    _)    = evalCheck env e
evalInfer _   (TermNat     n)         = Val.Nat n
evalInfer _   (TermFree    name)      = case builtins M.!? name of
                                        Just builtin -> builtin
                                        Nothing      -> Val.Free name
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
valueApp (Val.Lam _ body env)     arg  = evalCheck (arg : env) body
valueApp pap@(Val.Pap a _ _)      arg1 = case a of
                                         1 -> uncurry builtinApp $ collect (Val.Pap 0 pap arg1)
                                         _ -> Val.Pap (a - 1) pap arg1
valueApp v@(Val.VBuiltin builtin) arg  = case Val.arity builtin of
                                         1 -> builtinApp builtin [arg]
                                         a -> Val.Pap (a - 1) v arg
valueApp a b                           = Val.App a b

collect :: Val.Value -> (Val.Builtin, [Val.Value])
collect pap@(Val.Pap 0 _ _) = go pap []
  where go (Val.Pap _ (Val.VBuiltin b) x) xs = (b, x:xs)
        go (Val.Pap _ inner            x) xs = go inner (x:xs)
        go _                              _  = error "bug in collect"
collect _                   = error "collect only works with partial applications"

builtinApp :: Val.Builtin -> [Val.Value] -> Val.Value
builtinApp builtin args = case Val.arity builtin of
  2 -> let [a, b] = args in (Val.binOps M.! builtin) (a, b)
  1 -> let [a]    = args in (Val.unOps  M.! builtin)  a
  _ -> undefined

typeApp :: Val.Value -> Type -> Val.Value
typeApp (Val.TyLam _ body env) _ = evalCheck env (TermCheckInf body)
typeApp a b                      = Val.TyApp a b


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


