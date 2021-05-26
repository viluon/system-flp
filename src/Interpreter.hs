module Interpreter (
  main
, helper
) where

import System.IO ( stdout, hFlush )

import Syntax.AST
import Syntax.Command
import Context
import Eval ( eval )
import Parser.Parser ( parseExpr )
import Typechecking
import Utils

{-
let x = p in e

(\x -> e) p

-}

checkAndEval :: TermCheck -> Either Error String
checkAndEval term' = do
  tp' <- typeOf term'
  let tp   = desugarTypeSubst builtinTypes tp'
  let term = desugar builtinTypes term'
  () <- check [("Nat", HasKind Star)] term tp
  return $ show $ eval [] term

helper expr =
  case parseExpr expr of
    Left  _    -> error "cmd"
    Right term -> checkAndEval term



-- /\ g. ((\x -> x) :: T -> T)
--    type: forall g. T -> T

-- /\ T. (/\ g. ((\x -> x) :: T -> T))
--    type: forall T. (forall g. T -> T)

-- (/\ T. (/\ g. ((\x -> x) :: T -> T)))[Int]
--


-- >>> helper "(/\\ T. (/\\ g. (\\x -> x) :: T -> T))[Int]"

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
