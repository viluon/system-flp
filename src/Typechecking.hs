{-# LANGUAGE LambdaCase #-}

module Typechecking (
  check
, typeOf
, typeSubst
) where

import Syntax.AST
import Context
import Utils

check :: Context -> TermCheck -> Type -> Either Error ()
check ctx (TermCheckLam var body) tp = do (a, b) <- case tp of
                                            TyFun a b -> Right (a, b)
                                            _         -> Left $ "Expected type " ++ show tp ++ ", got function"
                                          check ((var, HasType a):ctx) body b
check ctx (TermCheckInf term)     tp = do inferred <- infer ctx term
                                          if tp == inferred then
                                            Right ()
                                          else
                                            Left $ "Expected " ++ show tp ++ ", got " ++ show inferred

-- (\x -> x) :: Nat

isValue :: (a, Info) -> Bool
isValue (_, HasType _) = True
isValue _              = False

infer :: Context -> TermInfer -> Either Error Type
infer ctx (TermAnn   term  tp)   = check ctx term tp >> pure tp
infer _   (TermNat   _)          = Right TyNat
infer _   (TermFree  name)       = Left $ "Undefined reference to " ++ show name -- unbound variable, TODO
infer ctx (TermBound n     name) = case filter isValue ctx !!? n of
                                     Just (_, HasType t) -> Right t
                                     Just (_, _)         -> error "bug in isValue"
                                     Nothing             -> Left $ "Undefined reference to " ++ show name
infer ctx (TermApp   lam   arg)  = do tp <- infer ctx lam
                                      (a, b) <- case tp of
                                        TyFun a b -> Right (a, b)
                                        _         -> Left $ "Expected function type, got " ++ show tp
                                      check ctx arg a
                                      pure b
infer ctx (TermTyLam tvar  body) = do tp <- infer ((tvar, HasKind Star):ctx) body
                                      pure $ TyForall tvar tp
infer ctx (TermTyApp tlam  targ) = do tp <- infer ctx tlam
                                      case tp of
                                        TyForall nm forAll -> Right $
                                          typeSubst {- FIXME this is an issue! We need to substitute in the entire AST -}
                                            nm targ forAll
                                        _                  -> Left $
                                          "Expected expression of forall type, got expression of type " ++ show tp


class HasType a where
  typeOf :: a -> Either Error Type

instance HasType TermCheck where
  typeOf (TermCheckLam _ _)  = Left "Cannot infer a type for an unannotated lambda abstraction"
  typeOf (TermCheckInf term) = typeOf term

-- >>> typeOf ()

instance HasType TermInfer where
  typeOf (TermAnn   _     tp)   = Right tp
  typeOf (TermNat   _)          = Right $ TyFree "Nat"
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


