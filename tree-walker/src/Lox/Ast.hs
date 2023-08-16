module Lox.Ast where

data Statement i
  = ExpressionStatement {expr :: Expression i}
  | PrintStatement {expr :: Expression i}
  | DeclareStatement {ident :: i, maybeExpr :: Maybe (Expression i)}
  | BlockStatement {stmts :: [Statement i]}
  | IfStatement
      { condition :: Expression i,
        then_ :: Statement i,
        else_ :: Maybe (Statement i)
      }
  | WhileStatement
      { condition :: Expression i,
        do_ :: Statement i
      }
  | ReturnStatement {expr :: Expression i}
  deriving (Show, Eq)

data Value i
  = Number Double
  | String String
  | Boolean Bool
  | Nil
  | Identifier i
  deriving (Show, Eq)

data Expression i
  = Binary {lhs :: Expression i, operator :: Operator, rhs :: Expression i}
  | Grouping {expr :: Expression i}
  | Literal {value :: Value i}
  | Unary {operator :: Operator, rhs :: Expression i}
  | Assign {ident :: i, expr :: Expression i}
  | Call {f :: Expression i, args :: [Expression i]}
  | Function {params :: [i], body :: Statement i}
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
