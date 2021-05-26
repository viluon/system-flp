module Interpreter (
  main
, helper
) where

import System.IO ( stdout, hFlush )
import Debug.Trace ( trace )

import Syntax.AST
import Syntax.Command
import Context
import Eval ( eval )
import Parser.Parser ( parseExpr )
import Typechecking
import Utils

checkAndEval :: TermCheck -> Either Error String
checkAndEval term' = do
  tp' <- typeOf term'
  let tp = typeSubst "Nat" TyNat tp'
  -- let term = TermCheckInf $ TermTyApp (TermTyLam "Nat" (TermAnn term' tp)) TyNat
  let term = term'
  () <- check [("Nat", HasKind Star)] term tp
  return $ show $ eval [] term

helper expr =
  case parseExpr expr of
    Left _     -> error "cmd"
    Right term | trace (show term) True -> checkAndEval term
    _ -> undefined


-- >>> helper "(/\\ T. (/\\ g. (\\x -> x) :: T -> T))[Int]"
-- Right "TyLam \"T\" (TermAnn (TermCheckLam \"x\" (TermCheckInf (TermBound 0 \"x\"))) (TyFun (TyFree \"T\") (TyFree \"T\"))) []"

-- >>> helper "(/\\ Nat . (\\x -> x) :: Nat -> Nat)[t]"
-- Right "Lam \"x\" (TermCheckInf (TermBound 0 \"x\")) []"

-- >>> helper "(/\\ t . (\\x -> x) :: String -> t)[String]"
-- Left "Expected TyFree \"t\", got TyFree \"String\""



main :: IO ()
main = go []
  where
    go ctx = do
      putStr "> "
      hFlush stdout
      line <- getLine
      case parseExpr line of
        Left cmd   -> case cmd of
          Quit        -> pure ()
          Assume ctx' -> go $ ctx ++ ctx'
        Right term -> putStrLn (either id id $ checkAndEval term) >> go ctx
