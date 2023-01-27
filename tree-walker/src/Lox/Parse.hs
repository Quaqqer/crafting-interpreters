module Lox.Parse () where

import Lox.Ast qualified as Ast
import Lox.Parser
import Lox.Token qualified as T

type Parser = Parser' T.Token

-- expression :: Parser Ast.Expression
-- expression =
--   literal
--
-- -- <|> unary
-- -- <|> binary
-- -- <|> grouping
--
-- literal :: Parser Ast.Expression
-- literal = number
--
-- number :: Parser Ast.Expression
-- number = do
--   t <- satisfy (\case T.Number _ -> True; _ -> False)
--   t
