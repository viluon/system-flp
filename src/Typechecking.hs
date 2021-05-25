{-# LANGUAGE LambdaCase #-}

module Typechecking (
  check
, typeOf
) where

import Syntax.AST
import Context
import Utils


check :: TermCheck -> Type -> Either Error ()
check (TermCheckLam var body) tp = _h
check (TermCheckInf term)     tp = _h'

class HasType a where
  typeOf :: Context -> a -> Either Error Type

instance HasType TermCheck where
  typeOf _   (TermCheckLam _ _)  = Left "Cannot infer a type for an unannotated lambda abstraction"
  typeOf ctx (TermCheckInf term) = typeOf ctx term

instance HasType TermInfer where
  typeOf _   (TermAnn   _     tp)   = Right tp
  typeOf _   (TermNat   _)          = Right TyNat
  typeOf _   (TermFree  name)       = Left $ "Undefined reference to " ++ name
  typeOf ctx (TermBound n     _)    = maybe (Left "invalid bound reference!") (convert . snd) $ ctx !!? n
                                    where convert (HasKind _)  = Left "invalid reference to type"
                                          convert (HasType tp) = Right tp
  typeOf ctx (TermApp   lam   _)    = typeOf ctx lam >>= \case
                                        (TyFun _ tp) -> Right tp
                                        tp           -> Left $ "Type " ++ show tp ++ " is not a function"
  typeOf ctx (TermTyLam name  body) = do tp <- typeOf ((name, HasKind Star) : ctx) body; pure $ TyForall name tp
  typeOf ctx (TermTyApp term  targ) = typeOf ctx term >>= \case
                                        (TyForall name body) -> Right $ typeSubst name targ body
                                        _                    -> Left ""

typeSubst :: Name -> Type -> Type -> Type
typeSubst nm arg (TyFree   var)    | nm == var = arg
typeSubst _  _   t@(TyFree _)                  = t
typeSubst _  _   TyNat                         = TyNat
typeSubst nm arg (TyFun    a   b)              = TyFun (typeSubst nm arg a) (typeSubst nm arg b)
typeSubst nm arg (TyForall var tp) | nm /= var = TyForall var (typeSubst nm arg tp)
typeSubst _  _   t@(TyForall _  _)             = t
