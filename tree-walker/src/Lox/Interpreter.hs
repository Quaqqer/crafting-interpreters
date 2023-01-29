{-# LANGUAGE PartialTypeSignatures #-}

module Lox.Interpreter
  ( Interpreter (..),
    InterpreterError (..),
    Value (..),
    iStmts,
    iStmt,
    iExpr,
    emptyState,
    State (..),
  )
where

import Control.Monad (ap)
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map
import Lox.Ast qualified as Ast

data Environment = Environment
  { vars :: Map String Value,
    parent :: Maybe Environment
  }

environmentGet :: String -> Environment -> Maybe Value
environmentGet ident env = case Map.lookup ident env.vars of
  Nothing -> case env.parent of
    Nothing -> Nothing
    Just parent -> environmentGet ident parent
  Just v -> Just v

environmentDeclare :: String -> Value -> Environment -> Environment
environmentDeclare ident value env = env {vars = Map.insert ident value env.vars}

environmentAssign :: String -> Value -> Environment -> Maybe Environment
environmentAssign ident value env =
  if Map.member ident env.vars
    then Just (env {vars = Map.insert ident value env.vars})
    else case env.parent of
      Just parent ->
        environmentAssign ident value parent >>= (\p -> Just env {parent = Just p})
      Nothing -> Nothing

data State = State
  { env :: Environment
  }

data Value
  = Number Double
  | Boolean Bool
  | String String
  | Nil

instance Show Value where
  show (Number n) = show n
  show (Boolean True) = "true"
  show (Boolean False) = "false"
  show (String s) = show s
  show Nil = "nil"

astValueToValue :: Ast.Value -> Interpreter Value
astValueToValue (Ast.Number d) = return (Number d)
astValueToValue (Ast.Boolean b) = return (Boolean b)
astValueToValue (Ast.String s) = return (String s)
astValueToValue Ast.Nil = return Nil
astValueToValue (Ast.Identifier ident) = envLookup ident

emptyState :: State
emptyState = State {env = Environment {vars = Map.empty, parent = Nothing}}

data InterpreterError
  = DivisionByZero
  | IncorrectType
  | ErrorMessage String
  | UndefinedVariable String
  deriving (Show)

newtype Interpreter a = Interpreter
  { run :: State -> IO (Either (InterpreterError, State) (a, State))
  }

deriving instance Functor Interpreter

instance Applicative Interpreter where
  pure a = Interpreter (\state -> return (Right (a, state)))
  (<*>) = ap

instance Monad Interpreter where
  return = pure

  x >>= f =
    Interpreter
      ( \state ->
          ( do
              res <- x.run state
              case res of
                Left err -> return (Left err)
                Right (a, state) -> (f a).run state
          )
      )

instance MonadIO Interpreter where
  liftIO io =
    Interpreter
      ( \state -> do
          res <- io
          return (Right (res, state))
      )

err :: InterpreterError -> Interpreter a
err e = Interpreter (\state -> return (Left (e, state)))

getState :: Interpreter State
getState = Interpreter (\state -> return (Right (state, state)))

setState :: State -> Interpreter ()
setState newState = Interpreter (\_ -> return (Right ((), newState)))

envLookup :: String -> Interpreter Value
envLookup ident = do
  state <- getState
  case environmentGet ident state.env of
    Just v -> return v
    Nothing -> err (UndefinedVariable ident)

envAssign :: String -> Value -> Interpreter ()
envAssign ident value = do
  state <- getState
  case environmentAssign ident value state.env of
    Nothing -> err (UndefinedVariable ident)
    Just env -> setState state {env}

envDeclare :: String -> Value -> Interpreter ()
envDeclare ident value = do
  state <- getState
  setState state {env = environmentDeclare ident value state.env}

getNumber :: Value -> Interpreter Double
getNumber (Number d) = return d
getNumber _ = err IncorrectType

getTruthy :: Value -> Interpreter Bool
getTruthy Nil = return False
getTruthy (Boolean b) = return b
getTruthy _ = return True

iNumber :: Ast.Expression -> Interpreter Double
iNumber expr = iExpr expr >>= getNumber

iBool :: Ast.Expression -> Interpreter Bool
iBool expr = iExpr expr >>= getTruthy

iStmts :: [Ast.Statement] -> Interpreter (Maybe Value)
iStmts [s] = iStmt s
iStmts (s : ss) = do
  _ <- iStmt s
  iStmts ss
iStmts [] = return Nothing

iStmt :: Ast.Statement -> Interpreter (Maybe Value)
iStmt Ast.PrintStatement {expr} = do
  res <- iExpr expr
  liftIO $ print res
  return Nothing
iStmt Ast.ExpressionStatement {expr} = do
  Just <$> iExpr expr
iStmt Ast.DeclareStatement {ident, maybeExpr} = do
  value <- case maybeExpr of
    Just expr -> iExpr expr
    Nothing -> return Nil
  envDeclare ident value
  return Nothing
iStmt Ast.AssignStatement {ident, expr} = do
  value <- iExpr expr
  envAssign ident value
  return Nothing

iExpr :: Ast.Expression -> Interpreter Value
iExpr Ast.Literal {value = astValue} = astValueToValue astValue
iExpr Ast.Grouping {expr} = iExpr expr
iExpr Ast.Unary {operator, rhs} = case operator of
  Ast.Minus -> Number . negate <$> (iExpr rhs >>= getNumber)
  Ast.Not -> Boolean . not <$> (iExpr rhs >>= getTruthy)
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
          (String l, String r) -> return (String (l ++ r))
          (Number l, Number r) -> return (Number (l + r))
          (_, _) -> err IncorrectType
        )
    addOp _ = error "Unexpected expression"

    numberOp Ast.Binary {lhs, operator, rhs} =
      Number <$> (op <$> iNumber lhs <*> iNumber rhs)
      where
        op = case operator of
          Ast.Minus -> (-)
          Ast.Multiplication -> (*)
          Ast.Division -> (/)
          _ -> error "Unexpected operator"
    numberOp _ = error "Unexpected expression"

    boolOp Ast.Binary {lhs, operator, rhs} =
      Boolean <$> (op <$> iNumber lhs <*> iNumber rhs)
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
        ( Boolean
            ( case operator of
                Ast.Equal -> eq
                Ast.InEqual -> not eq
                _ -> error "Unexpected operator"
            )
        )
    eqOp _ = error "Unexpected expression"

areEqual :: Value -> Value -> Interpreter Bool
areEqual (Number lhs) (Number rhs) = return (lhs == rhs)
areEqual (String lhs) (String rhs) = return (lhs == rhs)
areEqual lhs rhs = (==) <$> getTruthy lhs <*> getTruthy rhs
