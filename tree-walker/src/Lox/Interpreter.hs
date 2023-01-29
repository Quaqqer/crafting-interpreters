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
  | ErrorMessage String
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

getTruthy :: Ast.Value -> Interpreter Bool
getTruthy Ast.Nil = return False
getTruthy (Ast.Boolean b) = return b
getTruthy _ = return True

iNumber :: Ast.Expression -> Interpreter Double
iNumber expr = interpretExpression expr >>= getNumber

iBool :: Ast.Expression -> Interpreter Bool
iBool expr = interpretExpression expr >>= getTruthy

interpretExpression :: Ast.Expression -> Interpreter Ast.Value
interpretExpression Ast.Literal {value} = return value
interpretExpression Ast.Grouping {expr} = interpretExpression expr
interpretExpression Ast.Unary {operator, rhs} = case operator of
  Ast.Minus -> Ast.Number . negate <$> (interpretExpression rhs >>= getNumber)
  Ast.Not -> Ast.Boolean . not <$> (interpretExpression rhs >>= getTruthy)
  _ -> error "Not a unary operator"
interpretExpression expr@Ast.Binary {operator} =
  case operator of
    Ast.Plus -> numberOp expr
    Ast.Minus -> numberOp expr
    Ast.Multiplication -> numberOp expr
    Ast.Division -> numberOp expr
    Ast.Equal -> boolOp expr
    Ast.InEqual -> boolOp expr
    Ast.Greater -> boolOp expr
    Ast.GreaterEqual -> boolOp expr
    Ast.Less -> boolOp expr
    Ast.LessEqual -> boolOp expr
    _ -> error ("Operator " ++ show operator ++ " is not a binary operator")
  where
    numberOp Ast.Binary {lhs, operator, rhs} =
      Ast.Number <$> (op <$> iNumber lhs <*> iNumber rhs)
      where
        op = case operator of
          Ast.Plus -> (+)
          Ast.Minus -> (-)
          Ast.Multiplication -> (*)
          Ast.Division -> (/)
          _ -> error "Unexpected operator"
    numberOp _ = error "Unexpected expression"

    boolOp Ast.Binary {lhs, operator, rhs} =
      Ast.Boolean <$> (op <$> iBool lhs <*> iBool rhs)
      where
        op = case operator of
          Ast.Equal -> (==)
          Ast.InEqual -> (/=)
          Ast.Greater -> (>)
          Ast.GreaterEqual -> (>=)
          Ast.Less -> (<)
          Ast.LessEqual -> (<=)
          _ -> error "Unexpected operator"
    boolOp _ = error "Unexpected expression"
