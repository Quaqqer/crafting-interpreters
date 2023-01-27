module Lox.Token (Token (..), WithPos (..)) where

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
