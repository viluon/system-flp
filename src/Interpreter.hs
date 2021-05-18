module Interpreter (
  main
) where

import System.IO ( stdout, hFlush )
import Syntax.AST
import Syntax.Command
import Eval
import Parser.Parser ( parseExpr )

-- getLine :: RealWorld -> (String, RealWorld)

checkAndEval :: TermCheck -> IO ()
checkAndEval term = do
  print $ evalCheck [] term

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
        Right term -> checkAndEval term >> go ctx
