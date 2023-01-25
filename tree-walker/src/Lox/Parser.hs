{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lox.Parser () where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
import Control.Monad ((>=>))
import Data.Set (Set)
import Data.Set qualified as Set
import Prelude hiding (take, takeWhile)

data ParserState t = ParserState
  { rest :: [t],
    offset :: Int
  }
  deriving (Show)

data ParseError t = BasicError
  { offset :: Int,
    got :: Maybe (ErrorItem t),
    expected :: Set (ErrorItem t)
  }
  deriving (Show)

data ErrorItem t
  = Tokens [t]
  | Label String
  deriving (Show)

newtype Parser' t a = Parser
  { run ::
      ParserState t ->
      Either (ParseError t, ParserState t) (a, ParserState t)
  }

parse :: Parser' t a -> [t] -> Either (ParseError t) a
parse parser source =
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
          Left _e1 -> case r.run state of
            Right ok -> Right ok
            Left e2 -> Left e2
      )

-- where
--
-- mergeErr
--   (ParseExpected lEx, s1)
--   (ParseExpected rEx, s2) =
--     if s1.offset == s2.offset
--       then (ParseExpected (lEx ++ rEx), s1)
--       else firstErr (ParseExpected lEx, s1) (ParseExpected rEx, s2)
-- mergeErr lErr sErr = firstErr lErr sErr
--
-- firstErr (e1, s1) (e2, s2) =
--   if s1.offset >= s2.offset
--     then (e1, s1)
--     else (e2, s2)

infix 0 <?>

(<?>) :: Parser' t a -> String -> Parser' t a
parser <?> s =
  Parser
    ( \state -> case parser.run state of
        Left (_, state') ->
          Left
            (BasicError state.offset Nothing (Set.singleton (Label s)), state')
        Right r -> Right r
    )

satisfy :: (t -> Bool) -> Parser' t t
satisfy predicate =
  Parser
    ( \state -> case state.rest of
        [] -> Left (BasicError state.offset (Just (Label "eof")) Set.empty, state)
        (c : cs) ->
          if predicate c
            then
              Right
                ( c,
                  state
                    { rest = cs,
                      offset = state.offset + 1
                    }
                )
            else Left (BasicError state.offset (Just (Tokens [c])) Set.empty, state)
    )

token :: (t -> Bool) -> (Int -> Maybe t -> ParseError t) -> Parser' t t
token pred mkErr =
  Parser
    ( \state -> case state.rest of
        [] -> Left (mkErr state.offset Nothing, state)
        (c : cs) ->
          if pred c
            then Right (c, state {offset = state.offset + 1, rest = cs})
            else Left (mkErr state.offset (Just c), state)
    )

char :: Eq t => t -> Parser' t t
char c =
  token
    (== c)
    ( \offset got -> case got of
        Nothing -> BasicError offset (Just (Label "eof")) (Set.singleton (Tokens [c]))
        Just got -> BasicError offset (Just (Tokens [got])) (Set.singleton (Tokens [c]))
    )

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

string :: Eq t => [t] -> Parser' t [t]
string [] = pure []
string (s : ss) = do
  (:) <$> char s <*> string ss

eof :: Parser' Char ()
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
