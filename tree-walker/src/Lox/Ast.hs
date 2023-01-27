module Lox.Ast where

import Lox.Token (Token)

data Value
  = Number Double
  | String String
  | Boolean Bool
  | Nil

data Expression
  = Binary {lhs :: Expression, operator :: Token, rhs :: Expression}
  | Grouping {expr :: Expression}
  | Literal {value :: Value}
  | Unary {operator :: Token, rhs :: Expression}
