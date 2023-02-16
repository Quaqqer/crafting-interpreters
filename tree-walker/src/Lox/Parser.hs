{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lox.Parser
  ( ErrorBundle (..),
    ErrorItem (..),
    ParseError (..),
    ParseResult,
    ParserState (..),
    Parser' (..),
    (<?>),
    char,
    eof,
    err,
    getState,
    label,
    lookAhead,
    many,
    notFollowedBy,
    optional,
    parse',
    parse,
    parseFile,
    satisfy,
    sepBy,
    shouldFailOn,
    shouldFailWithError,
    shouldParse,
    shouldSucceedOn,
    showParseError,
    some,
    spec,
    string,
    surrounded,
    takeWhile,
    token,
  )
where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
import Control.Monad ((>=>))
import Data.List qualified as List
import Data.Maybe qualified
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Hspec
import Prelude hiding (take, takeWhile)

newtype Parser' t a = Parser
  { run ::
      ParserState t ->
      ParseResult t a
  }

data ParserState t = ParserState
  { rest :: [t],
    offset :: Int
  }
  deriving (Show)

data ErrorBundle t = ErrorBundle
  { source :: [t],
    fileName :: Maybe String,
    line :: Int,
    column :: Int,
    error :: ParseError t
  }

data ParseError t
  = BasicError
      { offset :: Int,
        got :: Maybe (ErrorItem t),
        expected :: Set (ErrorItem t)
      }
  | CustomError {offset :: Int, message :: String}

deriving instance Show t => Show (ParseError t)

data ErrorItem t
  = Tokens [t]
  | Label String
  deriving (Ord, Eq)

instance forall t. Show t => Show (ErrorItem t) where
  show (Tokens ts) = show ts
  show (Label l) = l

showParseError :: Show t => ParseError t -> String
showParseError BasicError {got, expected} =
  let msg = case got of
        Nothing -> case Set.toList expected of
          [] -> Nothing
          [e] -> Just ("expected " ++ show e)
          es -> Just ("expected one of " ++ List.intercalate ", " (map show es))
        Just got -> case Set.toList expected of
          [] -> Just ("unexpected " ++ show got)
          [e] -> Just ("got " ++ show got ++ " but expected " ++ show e)
          es -> Just ("got " ++ show got ++ " but expected one of " ++ List.intercalate ", " (map show es))
   in Data.Maybe.fromMaybe "Unknown parse error" msg
showParseError CustomError {message} = message

type ParseResult t a = Either (ParseError t, ParserState t) (a, ParserState t)

parse' :: Parser' t a -> [t] -> ParseResult t a
parse' p ts = p.run (ParserState {rest = ts, offset = 0})

parse :: Parser' t a -> [t] -> Either (ParseError t) a
parse parser source =
  case parse' parser source of
    Left (err, _state) -> Left err
    Right (a, _state) -> Right a

parseFile :: Parser' t a -> [t] -> Maybe String -> Either (ErrorBundle t) a
parseFile parser source fileName =
  case parser.run
    ( ParserState
        { rest = source,
          offset = 0
        }
    ) of
    Left (error, _state) ->
      Left
        ( ErrorBundle
            { source,
              error,
              line = 0,
              column = 0,
              fileName
            }
        )
    Right (a, _state) -> Right a

instance Functor (Parser' t) where
  fmap f parser =
    Parser
      (parser.run >=> (\(a, state') -> Right (f a, state')))

instance Applicative (Parser' t) where
  liftA2 f x y =
    Parser
      ( x.run
          >=> ( \(a, state') ->
                  y.run state'
                    >>= (\(b, state'') -> Right (f a b, state''))
              )
      )

  pure a = Parser (\state -> Right (a, state))

instance Monad (Parser' t) where
  l >>= r =
    Parser
      ( l.run >=> \(a, state') -> (r a).run state'
      )

  return = pure

instance (Ord t, Show t) => Alternative (Parser' t) where
  empty =
    Parser
      ( \state ->
          Left
            ( BasicError
                { offset = state.offset,
                  got = Nothing,
                  expected = Set.empty
                },
              state
            )
      )

  l <|> r =
    Parser
      ( \state -> case l.run state of
          Right ok -> Right ok
          Left (lErr, lState) -> case r.run state of
            Right ok -> Right ok
            Left (rErr, rState) ->
              Left
                ( if
                      | lErr.offset == rErr.offset -> (mergeErr lErr rErr, lState)
                      | lErr.offset < rErr.offset -> (rErr, rState)
                      | otherwise -> (lErr, lState)
                )
      )
    where
      mergeErr l r =
        if l.offset == r.offset
          then -- FIXME: Got are not necessarily equal, that might be a problem
            l {expected = l.expected `Set.union` r.expected}
          else error ("Assertion of same lengths failed: " ++ show l ++ show r)

infix 0 <?>

(<?>) :: Parser' t a -> String -> Parser' t a
parser <?> s =
  Parser
    ( \state -> case parser.run state of
        Left (err, state') ->
          let ts = List.take (err.offset - state'.offset + 1) state.rest
           in Left
                (BasicError state.offset (Just (Tokens ts)) (Set.singleton (Label s)), state')
        Right r -> Right r
    )

label :: String -> Parser' t a -> Parser' t a
label l parser = parser <?> l

satisfy :: (t -> Bool) -> Parser' t t
satisfy pred =
  token
    (\t -> if pred t then Just t else Nothing)
    Set.empty

token :: (t -> Maybe a) -> Set (ErrorItem t) -> Parser' t a
token test expected =
  Parser
    ( \state -> case state.rest of
        [] -> Left (BasicError {got = Just (Tokens []), expected, offset = state.offset}, state)
        (c : cs) ->
          case test c of
            Just a ->
              Right (a, state {offset = state.offset + 1, rest = cs})
            Nothing ->
              Left (BasicError {got = Just (Tokens [c]), expected, offset = state.offset + 1}, state)
    )

char :: Eq t => t -> Parser' t t
char c =
  token
    (\t -> if c == t then Just t else Nothing)
    (Set.singleton (Tokens [c]))

optional :: Parser' t a -> Parser' t (Maybe a)
optional parser =
  Parser
    ( \state -> case parser.run state of
        Left _ -> Right (Nothing, state)
        Right (a, state') -> Right (Just a, state')
    )

many :: Parser' t a -> Parser' t [a]
many parser =
  Parser
    ( \state -> case parser.run state of
        Right (a, state') -> Right (a : as, state'')
          where
            (as, state'') = case (many parser).run state' of
              Left _ -> error "Assertion that many always returns a right failed"
              Right r -> r
        Left _ -> Right ([], state)
    )

some :: Parser' t a -> Parser' t [a]
some parser = do
  first <- parser
  rest <- many parser
  return (first : rest)

takeWhile :: (Char -> Bool) -> Parser' Char String
takeWhile p = many (satisfy p)

lookAhead :: Parser' t a -> Parser' t a
lookAhead parser =
  Parser
    ( \state -> case parser.run state of
        Left err -> Left err
        Right (a, _) -> Right (a, state)
    )

notFollowedBy :: Parser' t a -> Parser' t ()
notFollowedBy parser =
  Parser
    ( \state -> case parser.run state of
        Left _ -> Right ((), state)
        Right (_, state) -> Left (BasicError state.offset Nothing Set.empty, state)
    )

surrounded :: Parser' t a -> Parser' t b -> Parser' t c -> Parser' t b
surrounded l p r = l *> p <* r

sepBy :: Parser' t a -> Parser' t b -> Parser' t [a]
sepBy p s = do
  first <- p
  rest <- many (s *> p)
  return (first : rest)

string :: Eq t => [t] -> Parser' t [t]
string = foldr (\c -> (<*>) ((:) <$> char c)) (pure [])

eof :: Parser' t ()
eof =
  Parser
    ( \state -> case state.rest of
        [] -> Right ((), state)
        (c : _) ->
          Left
            ( BasicError
                state.offset
                (Just (Tokens [c]))
                (Set.singleton (Label "eof")),
              state
            )
    )

getState :: Parser' t (ParserState t)
getState = Parser (\state -> Right (state, state))

err :: String -> Parser' t a
err msg = Parser (\s -> Left (CustomError {offset = s.offset, message = msg}, s))

shouldParse :: Show t => Show a => Eq a => Either (ParseError t) a -> a -> Expectation
shouldParse (Right l) r = l `shouldBe` r
shouldParse (Left err) expected =
  expectationFailure
    ("expected: " ++ show expected ++ "\nbut parsing failed with error:\n" ++ showParseError err)

shouldSucceedOn :: Show t => ([t] -> Either (ParseError t) a) -> [t] -> Expectation
shouldSucceedOn parse tokens = case parse tokens of
  Left err -> expectationFailure ("parsing failed with error:\n" ++ showParseError err)
  Right _ -> return ()

shouldFailOn :: Show a => Show t => ([t] -> Either (ParseError t) a) -> [t] -> Expectation
shouldFailOn parse tokens = case parse tokens of
  Right v -> expectationFailure ("parse was expected to fail but parsed:\n" ++ show v)
  Left _ -> return ()

shouldFailWithError :: Show t => Show a => Eq a => Either (ParseError t) a -> String -> Expectation
shouldFailWithError (Right v) _ = expectationFailure ("parse was expected to fail but parsed:\n" ++ show v)
shouldFailWithError (Left err) expected = showParseError err `shouldBe` expected

spec :: Spec
spec = describe "Lox.Parser" $ do
  it "parses empty input" $ do
    parse eof `shouldSucceedOn` ""
    parse eof "" `shouldParse` ()

    parse eof `shouldFailOn` "a"

  it "parses single characters correctly" $ do
    parse (char 'a') "a" `shouldParse` 'a'
    parse (char 'b') "a" `shouldFailWithError` "got \"a\" but expected \"b\""

  it "parses (many parser) correctly" $ do
    parse (many (char 'a')) "aaab" `shouldParse` "aaa"
    parse (many (char 'b')) "aaab" `shouldParse` ""

  it "parses (some parser) correctly" $ do
    parse (some (char 'a')) "aaab" `shouldParse` "aaa"
    parse (some (char 'b')) "aaab" `shouldFailWithError` "got \"a\" but expected \"b\""

  it "fails correctly with labels" $ do
    parse (char 'a' <?> "character a") "b"
      `shouldFailWithError` "got \"b\" but expected character a"

  it "parses alternatives correctly" $ do
    parse (many (char 'a' <|> char 'b' <|> char 'c')) "abcabb" `shouldParse` "abcabb"
    parse (char 'a' <|> char 'b' <|> char 'c') "x"
      `shouldFailWithError` "got \"x\" but expected one of \"a\", \"b\", \"c\""
    parse (char 'a' <|> char 'b' <|> char 'c' <?> "chars") "x"
      `shouldFailWithError` "got \"x\" but expected chars"

  it "gets the deepest error" $ do
    parse
      ( string "aab"
          <|> (List.singleton <$> (many (char 'a') >> char 'b'))
          <|> string "aab"
      )
      "aaaa"
      `shouldFailWithError` "got \"\" but expected \"b\""

    parse
      ( string "aa"
          >> ( string "aab"
                 <|> (List.singleton <$> (many (char 'a') >> char 'b'))
                 <|> string "aab"
             )
          <?> "test"
      )
      "aaaaaac"
      `shouldFailWithError` "got \"aa\" but expected test"
