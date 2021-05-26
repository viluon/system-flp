{
module Parser.Parser (parseExpr) where

import qualified Data.Map as M

import Control.Monad (unless, fail)
import Control.Monad.State hiding (fix)

import qualified Parser.Token as Tok
import Parser.Repairable
import Parser.Lexer
import Parser.Utils

import Syntax.Command
import Syntax.AST
import Context
}


%name parserAct
%tokentype { Tok.Token }
%error { parseError }
%monad { P }
%lexer { lexer } { Tok.EOF }
-- %expect 0


%token
  quit          { Tok.Quit }
  assume        { Tok.Assume }
  var           { Tok.Identifier $$ }
  nat           { Tok.NatLiteral $$ }
  lambda        { Tok.Lambda }
  typelambda    { Tok.TyLam }
  '::'          { Tok.DoubleColon }
  ':'           { Tok.Colon }
  '('           { Tok.LeftParen }
  ')'           { Tok.RightParen }
  '['           { Tok.LeftBracket }
  ']'           { Tok.RightBracket }
  '*'           { Tok.Star }
  '->'          { Tok.Arrow }
  forall        { Tok.Forall }
  '.'           { Tok.Dot }


%%
Program         ::  { Either Command TermCheck }
                :   Either(Command, TermCheck)                      { $1 }


TypedParams     ::  { [(String, Type)] }
                :   OneOrMany(TypedParam)                           { $1 }


TypedParam      ::  { (String, Type) }
                :   '(' var '::' Type ')'                           { ($2, $4) }

Either(a, b)    :: { Either a b }
                : a                                                 { Left  $1 }
                | b                                                 { Right $1 }

AppType         :: { Type }
                :   '[' Type ']'                                    { $2 }

TermInfer       ::  { TermInfer }
                :   var                                             { TermFree $1 }
                |   nat                                             { TermNat  $1 }
--                |   '(' TermInfer TermCheck ')'                     { $2 :@: $3 }
                |   TermInfer OneOrMany(Either(AppType, AppRight))    { foldl
                                                                        (\ l r -> case r of
                                                                          { Left  t    -> TermTyApp l t
                                                                          ; Right term -> TermApp l term })
                                                                        $1 $2 }
                |   '(' TermCheck '::' Type ')'                     { TermAnn $2 $4 }
                |   TermCheck '::' Type                             { TermAnn $1 $3 }
                -- NOTE: adding parens around removes some conflicts
                -- |   '(' lambda Params '->' TermInfer ')'       { foldr
                --                                                         (\ (par, type') body -> LamAnn par type' body)
                --                                                         $5
                --                                                         $3 }
                |   '(' typelambda var '.' TermInfer ')'            { TermTyLam $3 $5 }
                |   '(' TermInfer ')' {- %shift -}                  { $2 }

AppRight        ::  { TermCheck }
                :   var                                             { TermCheckInf $ TermFree $1 }
                |   nat                                             { TermCheckInf $ TermNat  $1 }
                |   '(' TermCheck '::' Type ')'                     { TermCheckInf $ TermAnn $2 $4 }
                -- NOTE: adding parens around removes 9 r/r conflict
                -- |   '(' lambda TypedParams '->' TermInfer ')'       { TermCheckInf $ foldr
                --                                                         (\ (par, type') body -> LamAnn par type' body)
                --                                                         $5
                --                                                         $3 }
                |   '(' TermInfer ')' {- %shift -}                  { TermCheckInf $2 }
                |   '(' lambda Params '->' TermCheck ')'            { foldr
                                                                        (\ arg body -> TermCheckLam arg body)
                                                                        $5
                                                                        $3 }
                |   '(' TermCheck ')'                               { $2 }


Params          ::  { [String] }
                :   OneOrMany(var)                                  { $1 }


TermCheck       ::  { TermCheck }
                :   TermInfer                                       { TermCheckInf $1 }
                |   '(' lambda Params '->' TermCheck ')'            { foldr
                                                                        (\ arg body -> TermCheckLam arg body)
                                                                        $5
                                                                        $3 }
                |   '(' TermCheck ')'                               { $2 }


Type            ::  { Type }
                :   var                                             { TyFree  $1 }
                |   var '->' Type                                   { TyFun (TyFree $1) $3 }
                |   forall var '.' Type                             { TyForall $2 $4 }
                |   '(' Type ')' '->' Type                          { TyFun $2 $5 }
                |   '(' Type ')'                                    { $2 }


Command         ::  { Command }
                : ':' ActualCommand                                   { $2 }

ActualCommand   ::  { Command }
                :   quit                                            { Quit }
                |   assume var '::' '*'                             { Assume [ ($2, HasKind Star) ] }
                |   assume var '::' Type                            { Assume [ ($2, HasType $4) ] }
                |   assume OneOrMany(AssumeWrapped)                 { Assume $2 }


AssumeWrapped   ::  { (Name, Info) }
                :   '(' var '::' '*' ')'                            { ($2, HasKind Star) }
                |   '(' var '::' Type ')'                           { ($2, HasType $4) }




NoneOrMany(tok)
                :   {- empty -}                                     { [] }
                |   tok NoneOrMany(tok)                             { $1 : $2 }

OneOrMany(tok)
                :   tok NoneOrMany(tok)                             { $1 : $2 }

{


parseError _ = do
  lno   <- getLineNo
  colno <- getColNo
  s     <- get
  error $ "Parse error on line " ++ show lno ++ ", column " ++ show colno ++ "." ++ "\n" ++ (show s)


parseExpr :: String -> Either Command TermCheck
parseExpr s =
  repair <$> evalP parserAct s
}
