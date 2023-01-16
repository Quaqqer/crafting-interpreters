module Lox.Scanner (Token (..), TokenType (..), scanTokens) where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
import Control.Monad ((>=>))

data TokenType
  = LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Identifier
  | String
  | Number
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | EOF
  deriving (Show)

data Token = Token
  { type_ :: TokenType,
    lexeme :: String,
    line :: Int
  }

instance Show Token where
  show Token {type_, lexeme} = show type_ ++ " " ++ lexeme

-- scanTokens :: String -> [TokenType]

data ParserState = ParserState
  { rest :: String,
    column :: Int,
    line :: Int,
    offset :: Int
  }
  deriving (Show)

data ParseError
  = ParseEOF
  | ParseUnexpectedChar [Char]
  | ParseUnknownErr
  | ParseExpected [String]
  deriving (Show)

newtype Parser a = Parser
  { runParser' ::
      ParserState ->
      Either (ParseError, ParserState) (a, ParserState)
  }

runParser :: Parser a -> String -> Either ParseError a
runParser parser source =
  case runParser'
    parser
    ( ParserState
        { rest = source,
          column = 0,
          line = 0,
          offset = 0
        }
    ) of
    Left (err, _state) -> Left err
    Right (a, _state) -> Right a

instance Functor Parser where
  fmap f parser =
    Parser
      (runParser' parser >=> (\(a, state') -> Right (f a, state')))

instance Applicative Parser where
  liftA2 f x y =
    Parser
      ( runParser' x
          >=> ( \(a, state') ->
                  runParser' y state'
                    >>= (\(b, state'') -> Right (f a b, state''))
              )
      )

  pure a = Parser (\state -> Right (a, state))

instance Monad Parser where
  l >>= r =
    Parser
      ( runParser' l >=> \(a, state') -> runParser' (r a) state'
      )

  return = pure

instance Alternative Parser where
  empty = Parser (\state -> Left (ParseUnknownErr, state))

  l <|> r =
    Parser
      ( \state -> case runParser' l state of
          Right ok -> Right ok
          Left e1 -> case runParser' r state of
            Right ok -> Right ok
            Left e2 -> Left (mergeErr e1 e2)
      )
    where
      mergeErr
        (ParseExpected lEx, s1)
        (ParseExpected rEx, s2) =
          if offset s1 == offset s2
            then (ParseExpected (lEx ++ rEx), s1)
            else firstErr (ParseExpected lEx, s1) (ParseExpected rEx, s2)
      mergeErr lErr sErr = firstErr lErr sErr

      firstErr (e1, s1) (e2, s2) =
        if offset s1 >= offset s2
          then (e1, s1)
          else (e2, s2)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
  Parser
    ( \state@ParserState {rest, line, column, offset} -> case rest of
        [] -> Left (ParseEOF, state)
        (c : cs) ->
          if f c
            then
              let newLine = c == '\n'
               in Right
                    ( 'a',
                      state
                        { rest = cs,
                          line = if newLine then line + 1 else line,
                          column = if newLine then 0 else column + 1,
                          offset = offset + 1
                        }
                    )
            else Left (ParseUnexpectedChar [c], state)
    )

optional :: Parser a -> Parser (Maybe a)
optional parser =
  Parser
    ( \state -> case runParser' parser state of
        Left _ -> Right (Nothing, state)
        Right (a, state') -> Right (Just a, state')
    )

scanTokens :: String -> [Token]
scanTokens source = []

(<?>) :: Parser a -> String -> Parser a
parser <?> s =
  Parser
    ( \state -> case runParser' parser state of
        Left (_, state') -> Left (ParseExpected [s], state')
        Right r -> Right r
    )
