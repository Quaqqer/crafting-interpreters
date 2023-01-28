{-# LANGUAGE ScopedTypeVariables #-}

module Lox.Lox (runFile, runPrompt) where

import GHC.Base (when)
import GHC.IO.Exception (ExitCode (ExitFailure))
import Lox.Interpreter (Interpreter (..), emptyState, interpretExpression, State(..))
import Lox.Parse (expression)
import Lox.Parser (eof, parse, showParseError)
import Lox.Scanner (scanTokens)
import Lox.Token (WithPos (inner))
import System.Exit (exitWith)
import System.IO (hFlush, isEOF, stdout)

runFile :: String -> IO ()
runFile f = do
  source <- readFile f
  newState <- run emptyState source

  when (newState.hadError) $ do
    exitWith (ExitFailure 65)

runPrompt :: State -> IO ()
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
      newState <- run state line

      -- Loop
      runPrompt emptyState

run :: State -> String -> IO State
run state source = do
  let tokens = parse (scanTokens <* eof) source
  case tokens of
    Left err -> do
      putStrLn "Could not parse tokens:"
      putStrLn (showParseError err)
      return state
    Right withPosTokens -> do
      let tokens = map (.inner) withPosTokens
      let ast = parse (expression <* eof) tokens
      case ast of
        Left err -> do
          putStrLn "Could not parse tokens:"
          putStrLn (showParseError err)
          return state
        Right ast -> do
          let res = (interpretExpression ast).run state
          case res of
            Right (val, state) -> do
              print val
              return state
            Left (err, _) -> do
              print err
              return state

printErr :: Int -> String -> IO ()
printErr line message = report line "" message

report :: Int -> String -> String -> IO ()
report line where_ message =
  putStrLn
    ( "[line "
        ++ show line
        ++ "] Error"
        ++ where_
        ++ ": "
        ++ message
    )
