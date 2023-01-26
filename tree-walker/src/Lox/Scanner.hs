module Lox.Scanner
  ( Token (..),
    WithPos (..),
    Scanner,
    scanTokens,
    spec,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Functor (($>))
import Data.List (singleton)
import Data.Maybe (fromMaybe)
import Lox.Parser hiding (spec)
import Test.Hspec
import Prelude hiding (take, takeWhile)
import Control.Monad (foldM)

data Token
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
  | Identifier String
  | String String
  | Number Double
  | And
  | Class
  | Else
  | FFalse
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | TTrue
  | Var
  | While
  | EOF
  deriving (Show, Eq, Ord)

data WithPos a = WithPos
  { start :: Int,
    end :: Int,
    inner :: a
  }

instance Show a => Show (WithPos a) where
  show WithPos {inner} = show inner

type Scanner = Parser' Char

number :: Scanner String
number =
  ( do
      h <- some (satisfy isDigit)
      t <- optional ((:) <$> char '.' <*> some (satisfy isDigit))
      return (h ++ fromMaybe "" t)
  )
    <?> "number"

scanTokens :: Scanner [WithPos Token]
scanTokens = spaceConsumer *> many scanToken <* eof

stringToken :: Scanner Token
stringToken =
  ( do
      _ <- char '"'
      content <- many ((string "\\\"" $> '"') <|> satisfy (/= '"'))
      _ <- char '"'
      return (String content)
  )
    <?> "string"

isWhitespace :: Char -> Bool
isWhitespace ' ' = True
isWhitespace '\n' = True
isWhitespace _ = False

lexeme :: Scanner a -> Scanner a
lexeme parser = parser <* spaceConsumer

spaceConsumer :: Scanner String
spaceConsumer = do
  s <- takeWhile isWhitespace <|> takeComment
  if s == ""
    then return ""
    else (s ++) <$> spaceConsumer

takeComment :: Scanner String
takeComment = do
  _ <- string "//"
  content <- takeWhile (/= '\n')
  return ("//" ++ content)

withPos :: Scanner a -> Scanner (WithPos a)
withPos parser = do
  start <- getState
  parsed <- parser
  end <- getState
  return WithPos {start = start.offset, end = end.offset, inner = parsed}

tokenStrings :: [([Char], Token)]
tokenStrings =
  [ ("(", LeftParen),
    (")", RightParen),
    ("{", LeftBrace),
    ("}", RightBrace),
    (",", Comma),
    (".", Dot),
    ("-", Minus),
    ("+", Plus),
    (";", Semicolon),
    ("*", Star),
    ("!=", BangEqual),
    ("!", Bang),
    ("==", EqualEqual),
    ("=", Equal),
    ("<=", LessEqual),
    ("<", Less),
    (">=", GreaterEqual),
    (">", Greater),
    ("/", Slash)
  ]

scanToken :: Scanner (WithPos Token)
scanToken =
  lexeme
    ( withPos
        ( operator
            <|> stringToken
            <|> Number . read <$> number
            <|> keyword
            <|> identifier
        )
    )

operator :: Scanner Token
operator = foldl1 (<|>) (map (\(s, t) -> string s $> t) tokenStrings) <?> "operator"

keywordStrings :: [(String, Token)]
keywordStrings =
  [ ("and", And),
    ("class", Class),
    ("else", Else),
    ("false", FFalse),
    ("for", For),
    ("fun", Fun),
    ("if", If),
    ("nil", Nil),
    ("or", Or),
    ("print", Print),
    ("return", Return),
    ("super", Super),
    ("this", This),
    ("true", TTrue),
    ("var", Var),
    ("while", While)
  ]

keyword :: Scanner Token
keyword =
  foldl1
    (<|>)
    (map (\(s, t) -> (string s <* notFollowedBy (satisfy isAlphaNum)) $> t) keywordStrings)
    <?> "operator"

initIdentifierChar :: Scanner Char
initIdentifierChar = satisfy (\c -> isAlpha c || c == '_')

identifierChar :: Scanner Char
identifierChar = satisfy (\c -> isAlphaNum c || c == '_')

identifier :: Scanner Token
identifier =
  Identifier
    <$> ((:) <$> initIdentifierChar <*> many identifierChar)
    <?> "identifier"

shouldParseToken :: Either (ParseError Char) (WithPos Token) -> Token -> Expectation
shouldParseToken (Right parsed) expected = parsed.inner `shouldBe` expected
shouldParseToken (Left err) expected =
  expectationFailure
    ("expected: " ++ show expected ++ "\nbut tokenizing failed with error:\n" ++ showParseError err)

shouldParseTokens :: Either (ParseError Char) [WithPos Token] -> [Token] -> Expectation
shouldParseTokens (Right parsed) expected = map (\p -> p.inner) parsed `shouldBe` expected
shouldParseTokens (Left err) expected =
  expectationFailure
    ("expected: " ++ show expected ++ "\nbut tokenizing failed with error:\n" ++ showParseError err)

spec :: Spec
spec = do
  describe "Lox.Scanner" $ do
    it "tokenizes operators correctly" $ do
      parse scanToken "/" `shouldParseToken` Slash
      parse scanToken "==" `shouldParseToken` EqualEqual
      parse scanToken "/" `shouldParseToken` Slash
      parse scanToken "/" `shouldParseToken` Slash
      parse scanToken "(" `shouldParseToken` LeftParen
      parse scanToken ")" `shouldParseToken` RightParen
      parse scanToken "{" `shouldParseToken` LeftBrace
      parse scanToken "}" `shouldParseToken` RightBrace
      parse scanToken "," `shouldParseToken` Comma
      parse scanToken "." `shouldParseToken` Dot
      parse scanToken "-" `shouldParseToken` Minus
      parse scanToken "+" `shouldParseToken` Plus
      parse scanToken ";" `shouldParseToken` Semicolon
      parse scanToken "*" `shouldParseToken` Star
      parse scanToken "!=" `shouldParseToken` BangEqual
      parse scanToken "!" `shouldParseToken` Bang
      parse scanToken "==" `shouldParseToken` EqualEqual
      parse scanToken "=" `shouldParseToken` Equal
      parse scanToken "<=" `shouldParseToken` LessEqual
      parse scanToken "<" `shouldParseToken` Less
      parse scanToken ">=" `shouldParseToken` GreaterEqual
      parse scanToken ">" `shouldParseToken` Greater
      parse scanToken "/" `shouldParseToken` Slash

    it "tokenizes keywords" $ do
      parse scanToken "and" `shouldParseToken` And
      parse scanToken "class" `shouldParseToken` Class
      parse scanToken "else" `shouldParseToken` Else
      parse scanToken "false" `shouldParseToken` FFalse
      parse scanToken "for" `shouldParseToken` For
      parse scanToken "fun" `shouldParseToken` Fun
      parse scanToken "if" `shouldParseToken` If
      parse scanToken "nil" `shouldParseToken` Nil
      parse scanToken "or" `shouldParseToken` Or
      parse scanToken "print" `shouldParseToken` Print
      parse scanToken "return" `shouldParseToken` Return
      parse scanToken "super" `shouldParseToken` Super
      parse scanToken "this" `shouldParseToken` This
      parse scanToken "true" `shouldParseToken` TTrue
      parse scanToken "var" `shouldParseToken` Var
      parse scanToken "while" `shouldParseToken` While

    it "tokenizes strings" $ do
      parse scanToken "\"hello there\"" `shouldParseToken` String "hello there"
      parse scanToken "\"\"" `shouldParseToken` String ""
      parse scanToken "\"\\\"\"" `shouldParseToken` String "\""

    it "tokenizes numbers" $ do
      parse scanToken "123" `shouldParseToken` Number 123
      parse scanToken "123.123" `shouldParseToken` Number 123.123
