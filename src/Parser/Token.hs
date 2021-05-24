module Parser.Token where

data Token
  = Identifier String
  | NatLiteral Int

  | Lambda
  | TyLam -- NEW
  | Colon
  | DoubleColon
  | LeftParen
  | RightParen
  | LeftBracket -- NEW
  | RightBracket -- NEW
  | Star
  | Arrow
  | Forall -- NEW
  | Dot -- NEW

  | Quit
  | Assume

  | EOF
  deriving (Show, Eq)
