module Lox.Interpreter (Interpreter (..), iExpr, emptyState, State (..)) where

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
iNumber expr = iExpr expr >>= getNumber

iBool :: Ast.Expression -> Interpreter Bool
iBool expr = iExpr expr >>= getTruthy

iExpr :: Ast.Expression -> Interpreter Ast.Value
iExpr Ast.Literal {value} = return value
iExpr Ast.Grouping {expr} = iExpr expr
iExpr Ast.Unary {operator, rhs} = case operator of
  Ast.Minus -> Ast.Number . negate <$> (iExpr rhs >>= getNumber)
  Ast.Not -> Ast.Boolean . not <$> (iExpr rhs >>= getTruthy)
  _ -> error "Not a unary operator"
iExpr expr@Ast.Binary {operator} =
  case operator of
    Ast.Plus -> addOp expr
    Ast.Minus -> numberOp expr
    Ast.Multiplication -> numberOp expr
    Ast.Division -> numberOp expr
    Ast.Greater -> boolOp expr
    Ast.GreaterEqual -> boolOp expr
    Ast.Less -> boolOp expr
    Ast.LessEqual -> boolOp expr
    Ast.Equal -> eqOp expr
    Ast.InEqual -> eqOp expr
    _ -> error ("Operator " ++ show operator ++ " is not a binary operator")
  where
    addOp Ast.Binary {lhs, operator = Ast.Plus, rhs} = do
      l <- iExpr lhs
      r <- iExpr rhs
      ( case (l, r) of
          (Ast.String l, Ast.String r) -> return (Ast.String (l ++ r))
          (Ast.Number l, Ast.Number r) -> return (Ast.Number (l + r))
          (_, _) -> err IncorrectType
        )
    addOp _ = error "Unexpected expression"

    numberOp Ast.Binary {lhs, operator, rhs} =
      Ast.Number <$> (op <$> iNumber lhs <*> iNumber rhs)
      where
        op = case operator of
          Ast.Minus -> (-)
          Ast.Multiplication -> (*)
          Ast.Division -> (/)
          _ -> error "Unexpected operator"
    numberOp _ = error "Unexpected expression"

    boolOp Ast.Binary {lhs, operator, rhs} =
      Ast.Boolean <$> (op <$> iNumber lhs <*> iNumber rhs)
      where
        op = case operator of
          Ast.Greater -> (>)
          Ast.GreaterEqual -> (>=)
          Ast.Less -> (<)
          Ast.LessEqual -> (<=)
          _ -> error "Unexpected operator"
    boolOp _ = error "Unexpected expression"

    eqOp Ast.Binary {lhs, operator, rhs} = do
      l <- iExpr lhs
      r <- iExpr rhs
      eq <- areEqual l r
      return
        ( Ast.Boolean
            ( case operator of
                Ast.Equal -> eq
                Ast.InEqual -> not eq
                _ -> error "Unexpected operator"
            )
        )
    eqOp _ = error "Unexpected expression"

areEqual :: Ast.Value -> Ast.Value -> Interpreter Bool
areEqual (Ast.Number lhs) (Ast.Number rhs) = return (lhs == rhs)
areEqual (Ast.String lhs) (Ast.String rhs) = return (lhs == rhs)
areEqual lhs rhs = (==) <$> getTruthy lhs <*> getTruthy rhs
