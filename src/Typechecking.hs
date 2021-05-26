{-# LANGUAGE LambdaCase #-}

module Typechecking (
  check
, infer
, desugar
, builtinTypes
, desugarTypeSubst
) where

import qualified Data.Map as M

import Syntax.AST
import Context
import HasType
import Utils


builtinTypes :: M.Map Name Type
builtinTypes = M.fromList [("Nat", TyNat)]


class Sweet a where
  desugar :: M.Map Name Type -> a -> a

instance Sweet TermCheck where
  desugar bi (TermCheckLam var body) = TermCheckLam var (desugar bi body)
  desugar bi (TermCheckInf term)     = TermCheckInf     (desugar bi term)

instance Sweet TermInfer where
  desugar mt x         | M.empty == mt = x
  desugar bi (TermAnn       term tp)   = TermAnn term (desugarTypeSubst bi tp)
  desugar _  n@(TermNat     _)         = n
  desugar _  v@(TermFree    _)         = v
  desugar _  v@(TermBound   _ _)       = v
  desugar _  b@(TermBuiltin _)         = b
  desugar bi (TermApp       lam  arg)  = TermApp (desugar bi lam) (desugar bi arg)
  desugar bi (TermTyLam     tvar body) = let bi' = if   M.member tvar bi
                                               then M.delete tvar bi
                                               else bi
                                     in TermTyLam tvar (desugar bi' body)
  desugar bi (TermTyApp     tlam targ) = TermTyApp (desugar bi tlam) (desugarTypeSubst bi targ)

desugarTypeSubst :: M.Map Name Type -> Type -> Type
desugarTypeSubst = flip (foldr (uncurry typeSubst)) . M.toList


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

isValue :: (a, Info) -> Bool
isValue (_, HasType _) = True
isValue _              = False

infer :: Context -> TermInfer -> Either Error Type
infer ctx (TermAnn   term  tp)   = check ctx term tp >> pure tp
infer _   (TermNat   _)          = Right TyNat
infer _   (TermBuiltin b)        = typeOf b
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
                                        TyForall nm innerType -> Right $ typeSubst nm targ innerType
                                        _                     -> Left $
                                          "Expected expression of forall type, got expression of type " ++ show tp
