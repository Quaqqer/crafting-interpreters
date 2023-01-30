module Lox.Ast where

data Statement
  = ExpressionStatement {expr :: Expression}
  | PrintStatement {expr :: Expression}
  | DeclareStatement {ident :: String, maybeExpr :: Maybe Expression}
  | BlockStatement {stmts :: [Statement]}
  | IfStatement
      { condition :: Expression,
        then_ :: Statement,
        else_ :: Maybe Statement
      }

data Value
  = Number Double
  | String String
  | Boolean Bool
  | Nil
  | Identifier String
  deriving (Show, Eq)

data Expression
  = Binary {lhs :: Expression, operator :: Operator, rhs :: Expression}
  | Grouping {expr :: Expression}
  | Literal {value :: Value}
  | Unary {operator :: Operator, rhs :: Expression}
  | Assign {ident :: String, expr :: Expression}
  deriving (Show, Eq)

data Operator
  = InEqual
  | Equal
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Minus
  | Plus
  | Multiplication
  | Division
  | Not
  | And
  | Or
  deriving (Show, Eq)
