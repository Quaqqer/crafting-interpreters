module Lox.Parse
  ( program,
    expression,
    spec,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Functor (($>), (<&>))
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Lox.Ast qualified as Ast
import Lox.Parser hiding (spec)
import Lox.Scanner qualified as S
import Lox.Token qualified as T
import Test.Hspec

type Parser = Parser' T.Token

type Statement = Ast.Statement String
type Expression = Ast.Expression String

program :: Parser [Statement]
program = many declaration <* eof

funDeclaration :: Parser Statement
funDeclaration = returnStatement <|> declaration

returnStatement :: Parser Statement
returnStatement = do
  _ <- char T.Return
  expr <- expression
  _ <- char T.Semicolon
  return Ast.ReturnStatement {expr}

declaration :: Parser Statement
declaration =
  declareFunStatement
    <|> declareStatement
    <|> statement

declareFunStatement :: Parser Statement
declareFunStatement = do
  _ <- char T.Fun
  ident <- identifier
  params <- parenthesized (identifier `sepBy` char T.Comma)
  body <- funBlock
  if length params > 255
    then err "Too many parameters for function"
    else return Ast.DeclareStatement {ident, maybeExpr = Just (Ast.Function {params, body})}

statement :: Parser Statement
statement =
  exprStatement
    <|> forStatement
    <|> ifStatement
    <|> printStatement
    <|> whileStatement
    <|> block

exprStatement :: Parser Statement
exprStatement = try (Ast.ExpressionStatement <$> expression <* char T.Semicolon)

forStatement :: Parser Statement
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

ifStatement :: Parser Statement
ifStatement = do
  _ <- char T.If
  _ <- char T.LeftParen
  condition <- expression
  _ <- char T.RightParen
  then_ <- statement
  else_ <- optional (char T.Else *> statement)
  return Ast.IfStatement {condition, then_, else_}

printStatement :: Parser Statement
printStatement = Ast.PrintStatement <$> (char T.Print *> expression <* char T.Semicolon)

whileStatement :: Parser Statement
whileStatement = do
  _ <- char T.While
  condition <- char T.LeftParen *> expression <* char T.RightParen
  do_ <- statement
  return Ast.WhileStatement {condition, do_}

block :: Parser Statement
block = do
  _ <- char T.LeftBrace
  stmts <- many declaration
  _ <- char T.RightBrace
  return Ast.BlockStatement {stmts}

funBlock :: Parser Statement
funBlock = do
  _ <- char T.LeftBrace
  stmts <- many declaration
  _ <- char T.RightBrace
  return Ast.BlockStatement {stmts}

declareStatement :: Parser Statement
declareStatement = do
  _ <- char T.Var
  ident <- identifier
  maybeExpr <- optional (char T.Equal *> expression)
  _ <- char T.Semicolon
  return Ast.DeclareStatement {ident, maybeExpr}

expression :: Parser Expression
expression = assign <?> "expression"

assign :: Parser Expression
assign = do
  let self =
        try
          ( do
              ident <- identifier
              _ <- char T.Equal
              expr <- self
              return Ast.Assign {ident, expr}
          )
          <|> logicOr
   in self

binaryExpr :: [T.Token] -> Parser Expression -> Parser Expression
binaryExpr ops next = do
  let self lhs =
        try
          ( do
              operator <- operators ops
              rhs <- next
              self (Ast.Binary {lhs, operator, rhs})
          )
          <|> return lhs
   in next >>= self

logicOr :: Parser Expression
logicOr =
  binaryExpr [T.Or] logicAnd
    <?> "or"

logicAnd :: Parser Expression
logicAnd =
  binaryExpr [T.And] equality
    <?> "and"

equality :: Parser Expression
equality =
  binaryExpr [T.BangEqual, T.EqualEqual] comparison
    <?> "equality"

comparison :: Parser Expression
comparison =
  binaryExpr [T.Less, T.LessEqual, T.Greater, T.GreaterEqual] term
    <?> "comparison"

term :: Parser Expression
term =
  binaryExpr [T.Minus, T.Plus] factor
    <?> "term"

factor :: Parser Expression
factor =
  binaryExpr [T.Star, T.Slash] unary
    <?> "factor"

unary :: Parser Expression
unary =
  try
    ( do
        operator <- operators [T.Minus, T.Bang]
        rhs <- expression
        return Ast.Unary {operator, rhs}
    )
    <|> call
    <?> "unary"

call :: Parser Expression
call =
  try
    ( do
        f <- primary
        args <- parenthesized (expression `sepBy` char T.Comma)
        if length args > 255
          then err "Too many args, a maximum of 255 is allowed"
          else return Ast.Call {f, args}
    )
    <|> primary
    <?> "call"

primary :: Parser Expression
primary = literal <|> grouping

literal :: Parser Expression
literal =
  number
    <|> eString
    <|> bool
    <|> nil
    <|> (Ast.Literal . Ast.Identifier <$> identifier)
    <?> "literal"

number :: Parser Expression
number =
  token
    ( \case
        T.Number d -> Just (Ast.Literal (Ast.Number d))
        _ -> Nothing
    )
    (Set.singleton (Label "number"))

eString :: Parser Expression
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

bool :: Parser Expression
bool =
  ( (char T.TTrue $> Ast.Literal (Ast.Boolean True))
      <|> (char T.FFalse $> Ast.Literal (Ast.Boolean False))
  )
    <?> "boolean"

nil :: Parser Expression
nil = char T.Nil $> Ast.Literal Ast.Nil <?> "nil"

grouping :: Parser Expression
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

parenthesized :: Parser' T.Token b -> Parser' T.Token b
parenthesized p = surrounded (char T.LeftParen) p (char T.RightParen)

shouldSucceedOn' :: Show a => Parser a -> String -> Expectation
shouldSucceedOn' p s = case parse S.scanTokens s of
  Left err -> expectationFailure ("scanning tokens failed with error:\n" ++ showParseError err)
  Right tokens -> case parse' p (map (.inner) tokens) of
    Left state -> expectationFailure ("parsing tokens failed with error:\n" ++ showParseError (getError state))
    Right (a, s) ->
      unless (null s.rest) (expectationFailure ("expected to parse fully but rest is:\n" ++ show s.rest ++ "\nparsed:\n" ++ show a))

shouldFailOn' :: Show a => Parser a -> String -> Expectation
shouldFailOn' p s = case parse S.scanTokens s of
  Left err -> expectationFailure ("scanning tokens failed with error:\n" ++ showParseError err)
  Right tokens -> case parse' p (map (.inner) tokens) of
    Left _ -> return ()
    Right (a, s) ->
      expectationFailure ("expected to fail parsing but parsed:\n" ++ show a ++ (if null s.rest then "" else "with rest:\n" ++ show s.rest))

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

    it "parses declarations" $ do
      declareStatement `shouldSucceedOn'` "var a = 2;"
      declareStatement `shouldSucceedOn'` "var b;"

    it "parses if statements" $ do
      ifStatement `shouldSucceedOn'` "if (true) print a;"

    it "parses multiple statements" $ do
      program `shouldSucceedOn'` "var a = 1; var b = 2; print b;"

    it "parses a function call" $ do
      call `shouldSucceedOn'` "f(1,2,3,4,5,6,7,8)"

-- call `shouldFailOn'` ("f(" ++ intercalate "," (map (const "x") [0 :: Integer .. 255]) ++ ")")
