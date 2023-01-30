module Lox.Parse
  ( program,
    expression,
    spec,
  )
where

import Control.Applicative ((<|>))
import Data.Functor (($>), (<&>))
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Lox.Ast qualified as Ast
import Lox.Parser hiding (spec)
import Lox.Token qualified as T
import Test.Hspec

type Parser = Parser' T.Token

program :: Parser [Ast.Statement]
program = many declaration <* eof

declaration :: Parser Ast.Statement
declaration =
  declareStatement
    <|> statement

statement :: Parser Ast.Statement
statement =
  exprStatement
    <|> forStatement
    <|> ifStatement
    <|> printStatement
    <|> whileStatement
    <|> block

exprStatement :: Parser Ast.Statement
exprStatement = Ast.ExpressionStatement <$> expression <* char T.Semicolon

forStatement :: Parser Ast.Statement
forStatement = do
  _ <- char T.For
  _ <- char T.LeftParen
  decl <- declareStatement <|> exprStatement
  condition <- optional expression
  _ <- char T.Semicolon
  rhs <- optional expression
  _ <- char T.RightParen
  do_ <- statement
  -- This is so ugly, but whatever
  return
    Ast.BlockStatement
      { stmts =
          [ decl,
            Ast.WhileStatement
              { condition = fromMaybe (Ast.Literal (Ast.Boolean True)) condition,
                do_ =
                  Ast.BlockStatement
                    [ do_,
                      fromMaybe (Ast.BlockStatement []) (rhs <&> Ast.ExpressionStatement)
                    ]
              }
          ]
      }

ifStatement :: Parser Ast.Statement
ifStatement = do
  _ <- char T.If
  _ <- char T.LeftParen
  condition <- expression
  _ <- char T.RightParen
  then_ <- statement
  else_ <- optional (char T.Else *> statement)
  return Ast.IfStatement {condition, then_, else_}

printStatement :: Parser Ast.Statement
printStatement = Ast.PrintStatement <$> (char T.Print *> expression <* char T.Semicolon)

whileStatement :: Parser Ast.Statement
whileStatement = do
  _ <- char T.While
  condition <- char T.LeftParen *> expression <* char T.RightParen
  do_ <- statement
  return Ast.WhileStatement {condition, do_}

block :: Parser Ast.Statement
block = do
  _ <- char T.LeftBrace
  stmts <- many declaration
  _ <- char T.RightBrace
  return Ast.BlockStatement {stmts}

declareStatement :: Parser Ast.Statement
declareStatement = do
  _ <- char T.Var
  ident <- identifier
  maybeExpr <- optional (char T.Equal *> expression)
  _ <- char T.Semicolon
  return Ast.DeclareStatement {ident, maybeExpr}

expression :: Parser Ast.Expression
expression = assign <?> "expression"

assign :: Parser Ast.Expression
assign = do
  let self =
        ( do
            ident <- identifier
            _ <- char T.Equal
            expr <- self
            return Ast.Assign {ident, expr}
        )
          <|> logicOr
   in self

binaryExpr :: [T.Token] -> Parser Ast.Expression -> Parser Ast.Expression
binaryExpr ops next = do
  let self lhs =
        ( do
            operator <- operators ops
            rhs <- next
            self (Ast.Binary {lhs, operator, rhs})
        )
          <|> return lhs
   in next >>= self

logicOr :: Parser Ast.Expression
logicOr =
  binaryExpr [T.Or] logicAnd
    <?> "or"

logicAnd :: Parser Ast.Expression
logicAnd =
  binaryExpr [T.And] equality
    <?> "and"

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
    <|> (Ast.Literal . Ast.Identifier <$> identifier)
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

identifier :: Parser String
identifier =
  token
    ( \case
        T.Identifier ident -> Just ident
        _ -> Nothing
    )
    (Set.singleton (Label "identifier"))

bool :: Parser Ast.Expression
bool =
  ( (char T.TTrue $> Ast.Literal (Ast.Boolean True))
      <|> (char T.FFalse $> Ast.Literal (Ast.Boolean False))
  )
    <?> "boolean"

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
tokenToOperator T.Or = Ast.Or
tokenToOperator T.And = Ast.And
tokenToOperator _ = error "Unexpected operator"

spec :: Spec
spec = do
  -- it "parses precedence correctly" $ do
  describe "Lox.Parse" $ do
    it "parses multiple of the same precedence correctly" $ do
      parse (expression <* eof) [T.Number 0, T.Slash, T.Number 1, T.Slash, T.Number 2]
        `shouldParse` Ast.Binary
          { lhs =
              Ast.Binary
                { lhs = Ast.Literal (Ast.Number 0),
                  operator = Ast.Division,
                  rhs = Ast.Literal (Ast.Number 1)
                },
            operator = Ast.Division,
            rhs =
              Ast.Literal (Ast.Number 2)
          }
