{-# LANGUAGE LambdaCase #-}

module Typechecking (
  check
, typeOf
) where

import Syntax.AST
import Context
import Utils


check :: TermCheck -> Type -> Either Error ()
check (TermCheckLam var body) tp = undefined
check (TermCheckInf term)     tp = undefined

class HasType a where
  typeOf :: a -> Either Error Type

instance HasType TermCheck where
  typeOf (TermCheckLam _ _)  = Left "Cannot infer a type for an unannotated lambda abstraction"
  typeOf (TermCheckInf term) = typeOf term

-- >>> typeOf ()

instance HasType TermInfer where
  typeOf (TermAnn   _     tp)   = Right tp
  typeOf (TermNat   _)          = Right TyNat
  typeOf (TermFree  name)       = Left $ "Undefined reference to " ++ name
  typeOf (TermBound n     _)    = error "the impossible happened!"
  typeOf (TermApp   lam   _)    = typeOf lam >>= \case
                                    (TyFun _ tp) -> Right tp
                                    tp           -> Left $ "Type " ++ show tp ++ " is not a function"
  typeOf (TermTyLam name  body) = do tp <- typeOf body; pure $ TyForall name tp
  typeOf (TermTyApp term  targ) = typeOf term >>= \case
                                    (TyForall name body) -> Right $ typeSubst name targ body
                                    _                    -> Left ""

typeSubst :: Name -> Type -> Type -> Type
typeSubst nm arg (TyFree   var)    | nm == var = arg
typeSubst _  _   t@(TyFree _)                  = t
typeSubst _  _   TyNat                         = TyNat
typeSubst nm arg (TyFun    a   b)              = TyFun (typeSubst nm arg a) (typeSubst nm arg b)
typeSubst nm arg (TyForall var tp) | nm /= var = TyForall var (typeSubst nm arg tp)
typeSubst _  _   t@(TyForall _  _)             = t


