module Lox.Ast where

import Lox.Token (Token)

data Value
  = Number Double
  | String String
  | Boolean Bool
  | Nil
  deriving (Show, Eq)

data Expression
  = Binary {lhs :: Expression, operator :: Operator, rhs :: Expression}
  | Grouping {expr :: Expression}
  | Literal {value :: Value}
  | Unary {operator :: Operator, rhs :: Expression}
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
  deriving (Show, Eq)
