module Lox.Parser where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
import Control.Monad ((>=>))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (singleton)
import Data.Maybe (fromMaybe)
import Prelude hiding (take, takeWhile)

data ParserState t = ParserState
  { rest :: [t],
    column :: Int,
    line :: Int,
    offset :: Int
  }
  deriving (Show)

data ParseError t
  = ParseEOF
  | ParseUnexpectedChar [t]
  | ParseUnknownErr
  | ParseExpected [[t]]
  | ParseExpectedEOF
  deriving (Show)

newtype Parser' t a = Parser
  { runParser' ::
      ParserState ->
      Either (ParseError, ParserState) (a, ParserState)
  }

type Scanner = Parser' Char

runParser :: Parser' t a -> [t] -> Either (ParseError t) a
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

instance Functor Parser t where
  fmap f parser =
    Parser
      (runParser' parser >=> (\(a, state') -> Right (f a, state')))

instance Applicative Parser t where
  liftA2 f x y =
    Parser
      ( runParser' x
          >=> ( \(a, state') ->
                  runParser' y state'
                    >>= (\(b, state'') -> Right (f a b, state''))
              )
      )

  pure a = Parser (\state -> Right (a, state))

instance Monad Parser t where
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

infix 0 <?>

(<?>) :: Parser a -> String -> Parser a
parser <?> s =
  Parser
    ( \state -> case runParser' parser state of
        Left (_, state') -> Left (ParseExpected [s], state')
        Right r -> Right r
    )

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
                    ( c,
                      state
                        { rest = cs,
                          line = if newLine then line + 1 else line,
                          column = if newLine then 0 else column + 1,
                          offset = offset + 1
                        }
                    )
            else Left (ParseUnexpectedChar [c], state)
    )

take :: Parser Char
take = satisfy (const True)

char :: Char -> Parser Char
char c = satisfy (== c)

optional :: Parser a -> Parser (Maybe a)
optional parser =
  Parser
    ( \state -> case runParser' parser state of
        Left _ -> Right (Nothing, state)
        Right (a, state') -> Right (Just a, state')
    )

many :: Parser a -> Parser [a]
many parser =
  Parser
    ( \state -> case runParser' parser state of
        Right (a, state') -> Right (a : as, state'')
          where
            Right (as, state'') = runParser' (many parser) state'
        Left _ -> Right ([], state)
    )

some :: Parser a -> Parser [a]
some parser = do
  first <- parser
  rest <- many parser
  return (first : rest)

takeWhile :: (Char -> Bool) -> Parser String
takeWhile p = many (satisfy p)

lookAhead :: Parser a -> Parser a
lookAhead parser =
  Parser
    ( \state -> case runParser' parser state of
        Left err -> Left err
        Right (a, _) -> Right (a, state)
    )

string :: String -> Parser String
string [] = pure []
string (s : ss) = do
  (:) <$> char s <*> string ss

eof :: Parser ()
eof =
  Parser
    ( \state@ParserState {rest} -> case rest of
        [] -> Right ((), state)
        _ -> Left (ParseExpectedEOF, state)
    )

number :: Parser String
number = do
  h <- some (satisfy isDigit)
  t <- optional (char '.' *> some (satisfy isDigit))
  return (h ++ fromMaybe "" t)

scanTokens :: String -> Either ParseError [WithPos Token]
scanTokens = runParser (spaceConsumer *> many scanToken <* eof)

stringToken :: Parser Token
stringToken = do
  _ <- char '"'
  content <- concat <$> many (string "\\\"" <|> (singleton <$> satisfy (/= '"')))
  _ <- char '"'
  return (String content)

isWhitespace :: Char -> Bool
isWhitespace ' ' = True
isWhitespace '\n' = True
isWhitespace _ = False

lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaceConsumer

spaceConsumer :: Parser String
spaceConsumer = do
  s <- takeWhile isWhitespace <|> takeComment
  if s == ""
    then return ""
    else (s ++) <$> spaceConsumer

takeComment = do
  _ <- string "//"
  content <- takeWhile (/= '\n')
  return ("//" ++ content)

getState :: Parser ParserState
getState = Parser (\state -> Right (state, state))

withPos :: Parser a -> Parser (WithPos a)
withPos parser = do
  start <- getState
  parsed <- parser
  return WithPos {linePos = line start, columnPos = column start, inner = parsed}

scanToken :: Parser (WithPos Token)
scanToken =
  lexeme
    ( withPos
        ( LeftParen <$ char '('
            <|> RightParen <$ char ')'
            <|> LeftBrace <$ char '{'
            <|> RightBrace <$ char '}'
            <|> Comma <$ char ','
            <|> Dot <$ char '.'
            <|> Minus <$ char '-'
            <|> Plus <$ char '+'
            <|> Semicolon <$ char ';'
            <|> Star <$ char '*'
            <|> BangEqual <$ string "!="
            <|> Bang <$ char '!'
            <|> EqualEqual <$ string "=="
            <|> Equal <$ char '='
            <|> LessEqual <$ string "!="
            <|> Less <$ char '<'
            <|> GreaterEqual <$ string "!="
            <|> Greater <$ char '>'
            <|> Slash <$ char '/'
            <|> stringToken
            <|> Number . read <$> number
            <|> keywordOrIdentifier
        )
    )

keywordOrIdentifier :: Parser Token
keywordOrIdentifier = do
  s <-
    (:)
      <$> satisfy (\c -> isAlpha c || c == '_')
      <*> many (satisfy (\c -> isAlphaNum c || c == '_'))
  return $ case s of
    "and" -> And
    "class" -> Class
    "else" -> Else
    "false" -> FFalse
    "for" -> For
    "fun" -> Fun
    "if" -> If
    "nil" -> Nil
    "or" -> Or
    "print" -> Print
    "return" -> Return
    "super" -> Super
    "this" -> This
    "true" -> TTrue
    "var" -> Var
    "while" -> While
    _ -> Identifier s
