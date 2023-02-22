{-# LANGUAGE ScopedTypeVariables #-}

module Lox.Lox (runFile, runPrompt) where

import Lox.Ast qualified as Ast
import Lox.Interpreter qualified as Interpreter
import Lox.Parse qualified as Parse
import Lox.Parser qualified as Parser
import Lox.Scanner (scanTokens)
import Lox.Token (WithPos (inner))
import System.IO (hFlush, isEOF, stdout)

data RunError
  = TokenizationError String
  | ParseError String
  | RuntimeError String

showRunError :: RunError -> String
showRunError (TokenizationError s) = "failed tokenization with the following error:\n" ++ s
showRunError (ParseError s) = "failed parsing with the following error:\n" ++ s
showRunError (RuntimeError s) = "runtime error: " ++ s

runFile :: String -> IO ()
runFile f = do
  source <- readFile f
  state <- Interpreter.emptyState
  newState <- run state source

  case newState of
    Left err -> do
      putStrLn (showRunError err)
    Right _ -> return ()

runPrompt :: Interpreter.State -> IO ()
runPrompt state = do
  putStr "> "
  hFlush stdout

  done <- isEOF
  if done
    then do
      putStrLn ""
      return ()
    else do
      line <- getLine
      result <- run state line

      case result of
        Left err -> do
          putStrLn (showRunError err)
          runPrompt state
        Right (val, newState) -> do
          maybe (return ()) print val
          runPrompt newState

run ::
  Interpreter.State ->
  String ->
  IO (Either RunError (Maybe Interpreter.Value, Interpreter.State))
run state source = do
  case parse source of
    Left err -> return (Left err)
    Right ast -> do
      res <- (Interpreter.iStmts ast).run state
      case res of
        Left (err, _) -> return (Left (RuntimeError (show err)))
        Right res -> return (Right res)

parse :: String -> Either RunError [Ast.Statement]
parse s = case Parser.parse (scanTokens <* Parser.eof) s of
  Left err -> Left (TokenizationError (Parser.showParseError err))
  Right withPosTokens ->
    let tokens = map (.inner) withPosTokens
     in case Parser.parse Parse.program tokens of
          Left err -> Left (ParseError (Parser.showParseError err))
          Right ast -> Right ast
