module Parser.Repairable(repair)
where

import Syntax.AST
import Data.List (elemIndex)
import qualified Data.Map as M

class Repairable a where
  repair :: a -> a


instance Repairable TermCheck where
  repair lambda = repairCheck lambda []


instance Repairable TermInfer where
  repair lambda = repairInfer lambda []


repairCheck :: TermCheck -> [String] -> TermCheck
repairCheck (TermCheckLam par body) context
  = TermCheckLam par $ repairCheck body (par : context)
repairCheck (TermCheckInf term) context
  = TermCheckInf $ repairInfer term context


repairInfer :: TermInfer -> [String] -> TermInfer
repairInfer (TermAnn term tp) context
  = TermAnn (repairCheck term context) tp
repairInfer t@(TermNat   _)   _ = t
repairInfer t@(TermBound i n) _ = t
repairInfer t@(TermBuiltin _) _ = t
repairInfer t@(TermFree name) context
  = case elemIndex name context of
      Just ind -> TermBound ind name
      Nothing  -> maybe t TermBuiltin (M.lookup name builtinOps)
repairInfer (TermApp left right) context
  = TermApp (repairInfer left context) (repairCheck right context)
repairInfer (TermTyLam nm term)   context
  = TermTyLam nm (repairInfer term context)
repairInfer (TermTyApp tlam targ) context
  = TermTyApp (repairInfer tlam context) targ

-- repairInfer (LamAnn par tp body) context
--   = LamAnn par tp $ repairInfer body (par : context)

