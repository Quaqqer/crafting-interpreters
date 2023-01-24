module Lox.Parser where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
import Control.Monad ((>=>))
import Prelude hiding (take, takeWhile)

data ParserState t = ParserState
  { rest :: [t],
    offset :: Int
  }
  deriving (Show)

data ParseError t
  = ParseEOF
  | ParseUnexpectedChar [t]
  | ParseUnknownErr
  | ParseExpected [String]
  | ParseExpectedEOF
  deriving (Show)

newtype Parser' t a = Parser
  { run ::
      ParserState t ->
      Either (ParseError t, ParserState t) (a, ParserState t)
  }

runParser :: Parser' t a -> [t] -> Either (ParseError t) a
runParser parser source =
  case parser.run
    ( ParserState
        { rest = source,
          offset = 0
        }
    ) of
    Left (err, _state) -> Left err
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

instance Alternative (Parser' t) where
  empty = Parser (\state -> Left (ParseUnknownErr, state))

  l <|> r =
    Parser
      ( \state -> case l.run state of
          Right ok -> Right ok
          Left e1 -> case r.run state of
            Right ok -> Right ok
            Left e2 -> Left (mergeErr e1 e2)
      )
    where
      mergeErr
        (ParseExpected lEx, s1)
        (ParseExpected rEx, s2) =
          if s1.offset == s2.offset
            then (ParseExpected (lEx ++ rEx), s1)
            else firstErr (ParseExpected lEx, s1) (ParseExpected rEx, s2)
      mergeErr lErr sErr = firstErr lErr sErr

      firstErr (e1, s1) (e2, s2) =
        if s1.offset >= s2.offset
          then (e1, s1)
          else (e2, s2)

infix 0 <?>

(<?>) :: Parser' t a -> String -> Parser' t a
parser <?> s =
  Parser
    ( \state -> case parser.run state of
        Left (_, state') -> Left (ParseExpected [s], state')
        Right r -> Right r
    )

satisfy :: (t -> Bool) -> Parser' t t
satisfy predicate =
  Parser
    ( \state@ParserState {rest, offset} -> case rest of
        [] -> Left (ParseEOF, state)
        (c : cs) ->
          if predicate c
            then
              Right
                ( c,
                  state
                    { rest = cs,
                      offset = offset + 1
                    }
                )
            else Left (ParseUnexpectedChar [c], state)
    )

take :: Parser' Char Char
take = satisfy (const True)

char :: Char -> Parser' Char Char
char c = satisfy (== c)

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
            Right (as, state'') = (many parser).run state'
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

string :: String -> Parser' Char String
string [] = pure []
string (s : ss) = do
  (:) <$> char s <*> string ss

eof :: Parser' Char ()
eof =
  Parser
    ( \state@ParserState {rest} -> case rest of
        [] -> Right ((), state)
        _ -> Left (ParseExpectedEOF, state)
    )

getState :: Parser' t (ParserState t)
getState = Parser (\state -> Right (state, state))
