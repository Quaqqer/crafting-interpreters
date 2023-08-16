{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Lox.Resolver (runResolver) where

import Control.Monad.Except qualified as Except
import Control.Monad.State qualified as State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Traversable (for)
import Lox.Ast (Statement (..))
import Lox.Ast qualified as Ast

data State = State
  { scopes :: [Scope]
  }

type Scope = Map String Ident

data Err

type Resolver = State.StateT State (Except.Except Err)

data Ident
  = Local Int
  | Global String

runResolver :: [Ast.Statement String] -> Either Err [Ast.Statement Ident]
runResolver ast = Except.runExcept $ State.evalStateT (resolver ast) s
  where
    s = State {scopes = []}

resolver :: [Ast.Statement String] -> Resolver [Ast.Statement Ident]
resolver = mapM resolveStatement

resolveStatement :: Ast.Statement String -> Resolver (Ast.Statement Ident)
resolveStatement Ast.ExpressionStatement {expr} = do
  expr' <- resolveExpr expr
  return Ast.ExpressionStatement {expr = expr'}
resolveStatement Ast.ReturnStatement {expr} = do
  expr' <- resolveExpr expr
  return Ast.ReturnStatement {expr = expr'}
resolveStatement Ast.PrintStatement {expr} = do
  expr' <- resolveExpr expr
  return Ast.PrintStatement {expr = expr'}
resolveStatement Ast.DeclareStatement {ident, maybeExpr} = do
  maybeExpr' <- for maybeExpr resolveExpr
  ident' <- insertIdent ident
  return Ast.DeclareStatement {ident = ident', maybeExpr = maybeExpr'}
resolveStatement Ast.BlockStatement {stmts} = do
  enterScope
  stmts' <- mapM resolveStatement stmts
  exitScope
  return Ast.BlockStatement {stmts = stmts'}
resolveStatement Ast.IfStatement {condition, then_, else_} = do
  condition' <- resolveExpr condition
  then_' <- resolveStatement then_
  else_' <- for else_ resolveStatement
  return Ast.IfStatement {condition = condition', then_ = then_', else_ = else_'}
resolveStatement Ast.WhileStatement {condition, do_} = do
  condition' <- resolveExpr condition
  do_' <- resolveStatement do_
  return Ast.WhileStatement {condition = condition', do_ = do_'}

exitScope :: Resolver ()
exitScope = do
  s <- State.get
  let s' = s {scopes = tail s.scopes}
  State.put s'

enterScope :: Resolver ()
enterScope = do
  s <- State.get
  let s' = s {scopes = Map.empty : s.scopes}
  State.put s'

resolveExpr :: Ast.Expression String -> Resolver (Ast.Expression Ident)
resolveExpr Ast.Binary {lhs, operator, rhs} = do
  lhs' <- resolveExpr lhs
  rhs' <- resolveExpr rhs
  return Ast.Binary {lhs = lhs', operator, rhs = rhs'}
resolveExpr Ast.Grouping {expr} = do
  expr' <- resolveExpr expr
  return Ast.Grouping {expr = expr'}
resolveExpr Ast.Literal {value} = do
  value' <- resolveValue value
  return Ast.Literal {value = value'}
resolveExpr Ast.Unary {operator, rhs} = do
  rhs' <- resolveExpr rhs
  return Ast.Unary {operator, rhs = rhs'}
resolveExpr Ast.Assign {ident, expr} = do
  ident' <- resolveIdent ident
  expr' <- resolveExpr expr
  return Ast.Assign {ident = ident', expr = expr'}
resolveExpr Ast.Call {f, args} = do
  f' <- resolveExpr f
  args' <- mapM resolveExpr args
  return Ast.Call {f = f', args = args'}
resolveExpr Ast.Function {params, body} = do
  scoped $ do
    params' <- mapM insertIdent params
    body' <- resolveStatement body
    return Ast.Function {params = params', body = body'}

scoped :: Resolver a -> Resolver a
scoped r = enterScope *> r <* exitScope

resolveValue :: Ast.Value String -> Resolver (Ast.Value Ident)
resolveValue (Ast.Number d) = return (Ast.Number d)
resolveValue (Ast.String s) = return (Ast.String s)
resolveValue (Ast.Boolean b) = return (Ast.Boolean b)
resolveValue Ast.Nil = return Ast.Nil
resolveValue (Ast.Identifier i) = Ast.Identifier <$> resolveIdent i

insertIdent :: String -> Resolver Ident
insertIdent = undefined

resolveIdent :: String -> Resolver Ident
resolveIdent = undefined
