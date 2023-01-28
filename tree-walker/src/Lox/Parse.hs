module Lox.Parse (expression, spec) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Set qualified as Set
import Lox.Ast qualified as Ast
import Lox.Parser hiding (spec)
import Lox.Token qualified as T
import Test.Hspec

type Parser = Parser' T.Token

expression :: Parser Ast.Expression
expression = equality <?> "expression"

binaryExpr :: [T.Token] -> Parser Ast.Expression -> Parser Ast.Expression
binaryExpr ops next =
  let self =
        ( do
            lhs <- next
            operator <- operators ops
            rhs <- self
            return Ast.Binary {lhs, operator, rhs}
        )
          <|> next
   in self

equality :: Parser Ast.Expression
equality =
  binaryExpr [T.BangEqual, T.EqualEqual] comparison
    <?> "equality"

comparison :: Parser Ast.Expression
comparison =
  binaryExpr [T.Less, T.LessEqual, T.Greater, T.GreaterEqual] term
    <?> "comparison"

term :: Parser Ast.Expression
term =
  binaryExpr [T.Minus, T.Plus] factor
    <?> "term"

factor :: Parser Ast.Expression
factor =
  binaryExpr [T.Star, T.Slash] unary
    <?> "factor"

unary :: Parser Ast.Expression
unary =
  ( do
      operator <- operators [T.Minus, T.Bang]
      rhs <- expression
      return Ast.Unary {operator, rhs}
  )
    <|> primary
    <?> "unary"

primary :: Parser Ast.Expression
primary = literal <|> grouping

literal :: Parser Ast.Expression
literal =
  number
    <|> eString
    <|> bool
    <|> nil
    <?> "literal"

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
nil = char T.Nil $> Ast.Literal Ast.Nil <?> "nil"

grouping :: Parser Ast.Expression
grouping = do
  _ <- char T.LeftParen
  expr <- expression
  _ <- char T.RightParen
  return Ast.Grouping {expr}

operators :: [T.Token] -> Parser Ast.Operator
operators ts = tokenToOperator <$> foldl1 (<|>) (map char ts)

tokenToOperator :: T.Token -> Ast.Operator
tokenToOperator T.BangEqual = Ast.InEqual
tokenToOperator T.EqualEqual = Ast.Equal
tokenToOperator T.Less = Ast.Less
tokenToOperator T.LessEqual = Ast.LessEqual
tokenToOperator T.Greater = Ast.Greater
tokenToOperator T.GreaterEqual = Ast.GreaterEqual
tokenToOperator T.Minus = Ast.Minus
tokenToOperator T.Plus = Ast.Plus
tokenToOperator T.Star = Ast.Multiplication
tokenToOperator T.Slash = Ast.Division
tokenToOperator T.Bang = Ast.Not
tokenToOperator _ = error "Unexpected operator"

spec :: Spec
spec = do
  -- it "parses precedence correctly" $ do
  describe "Lox.Parse" $ do
    it "parses multiple of the same precedence correctly" $ do
      parse (expression <* eof) [T.Number 0, T.Slash, T.Number 0, T.Slash, T.Number 0]
        `shouldParse` Ast.Binary
          { lhs = Ast.Literal (Ast.Number 0),
            operator = Ast.Division,
            rhs =
              Ast.Binary
                { lhs = Ast.Literal (Ast.Number 0),
                  operator = Ast.Division,
                  rhs = Ast.Literal (Ast.Number 0)
                }
          }
