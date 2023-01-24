module Lox.Ast where

import Lox.Scanner (Token (..))

data Value

data Expression
  = Binary {lhs :: Expression, operator :: Token, rhs :: Expression}
  | Grouping {expr :: Expression}
  | Literal {value :: Value}
  | Unary {operator :: Token, rhs :: Expression}
