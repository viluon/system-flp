module Interpreter (
  main
) where

import System.IO ( stdout, hFlush )
import Data.Either

import Syntax.AST
import Syntax.Command
import Eval ( eval )
import Parser.Parser ( parseExpr )
import Typechecking
import Utils

checkAndEval :: TermCheck -> Either Error String
checkAndEval term = do
  tp <- typeOf term
  () <- check term tp
  return $ show $ eval [] term

helper expr =
  case parseExpr expr of
    Left _     -> error "cmd"
    Right term -> typeOf term


-- >>> helper "(/\\ T. (/\\ T. (\\x -> x) :: T))[Int]"
-- Right (TyForall "T" (TyFree "T"))

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
