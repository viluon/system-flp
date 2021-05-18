module Interpreter (
  main
) where

import System.IO ( stdout, hFlush )
import Parser.Utils
import Syntax.AST
import Syntax.Command
import Value
import Eval

parseExpr :: String -> Either Command TermCheck
parseExpr = undefined

-- getLine :: RealWorld -> (String, RealWorld)

checkAndEval :: TermCheck -> IO ()
checkAndEval term = do
  let value = evalCheck [] term
  print value

main :: IO ()
main = go
  where
    go = do
      putStr "> "
      hFlush stdout
      line <- getLine
      case parseExpr line of
        Left cmd   -> case cmd of
          Quit -> pure ()
        Right term -> checkAndEval term >> go
