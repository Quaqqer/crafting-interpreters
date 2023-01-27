module Lox.Parse (expression) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Set qualified as Set
import Lox.Ast qualified as Ast
import Lox.Parser
import Lox.Token qualified as T

type Parser = Parser' T.Token

expression :: Parser Ast.Expression
expression =
  literal
    <|> unary
    <|> binary
    <|> grouping

literal :: Parser Ast.Expression
literal =
  number
    <|> eString
    <|> bool
    <|> nil

number :: Parser Ast.Expression
number =
  token
    ( \case
        T.Number d -> Just (Ast.Literal (Ast.Number d))
        _ -> Nothing
    )
    (Set.singleton (Label "number"))

eString :: Parser Ast.Expression
eString =
  token
    ( \case
        T.String s -> Just (Ast.Literal (Ast.String s))
        _ -> Nothing
    )
    (Set.singleton (Label "string"))

bool :: Parser Ast.Expression
bool =
  char T.TTrue $> Ast.Literal (Ast.Boolean True)
    <|> char T.FFalse $> Ast.Literal (Ast.Boolean False)

nil :: Parser Ast.Expression
nil = char T.Nil $> Ast.Literal Ast.Nil

unary :: Parser Ast.Expression
unary = do
  operator <- char T.Minus <|> char T.Bang
  rhs <- expression
  return Ast.Unary {operator, rhs}

binaryOperator :: Parser T.Token
binaryOperator = foldl1 (<|>) (map char opTokens)
  where
    opTokens =
      [ T.EqualEqual,
        T.BangEqual,
        T.Less,
        T.LessEqual,
        T.Greater,
        T.GreaterEqual,
        T.Plus,
        T.Minus,
        T.Star,
        T.Slash
      ]

binary :: Parser Ast.Expression
binary = do
  lhs <- expression
  operator <- binaryOperator
  rhs <- expression
  return Ast.Binary {lhs, operator, rhs}

grouping :: Parser Ast.Expression
grouping = do
  _ <- char T.LeftParen
  expr <- expression
  _ <- char T.RightParen
  return Ast.Grouping {expr}
