module Lox.Interpreter (Interpreter (..), interpretExpression, emptyState, State (..)) where

import Control.Monad (ap, (>=>))
import Lox.Ast qualified as Ast

data State = State
  { hadError :: Bool
  }

emptyState :: State
emptyState = State {hadError = False}

data InterpreterError
  = DivisionByZero
  | IncorrectType
  deriving (Show)

newtype Interpreter a = Interpreter
  { run :: State -> Either (InterpreterError, State) (a, State)
  }

deriving instance Functor Interpreter

instance Applicative Interpreter where
  pure a = Interpreter (\state -> Right (a, state))
  (<*>) = ap

instance Monad Interpreter where
  return = pure

  x >>= f = Interpreter (x.run >=> (\(a, state) -> (f a).run state))

err :: InterpreterError -> Interpreter a
err e = Interpreter (\state -> Left (e, state))

getNumber :: Ast.Value -> Interpreter Double
getNumber (Ast.Number d) = return d
getNumber _ = err IncorrectType

getBool :: Ast.Value -> Interpreter Bool
getBool (Ast.Boolean b) = return b
getBool _ = err IncorrectType

interpretExpression :: Ast.Expression -> Interpreter Ast.Value
interpretExpression Ast.Literal {value} = return value
interpretExpression Ast.Grouping {expr} = interpretExpression expr
interpretExpression Ast.Unary {operator = Ast.Minus, rhs} =
  Ast.Number . negate <$> (interpretExpression rhs >>= getNumber)
interpretExpression Ast.Binary {lhs, operator, rhs} =
  case op operator of
    Right f ->
      Ast.Number . (uncurry f) <$> ((,) <$> (interpretExpression lhs >>= getNumber) <*> (interpretExpression rhs >>= getNumber))
    Left () -> err IncorrectType
  where
    op Ast.Plus = Right (+)
    op Ast.Minus = Right (-)
    op Ast.Multiplication = Right (*)
    op Ast.Division = Right (/)
    op _ = Left ()
