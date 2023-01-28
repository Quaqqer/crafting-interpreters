module Lox.Cli (cli) where

import Lox.Lox (runFile, runPrompt)
import Lox.Interpreter (emptyState)
import Options.Applicative

data Options = Options
  { file :: Maybe String
  }

parser :: Parser Options
parser =
  Options
    <$> optional (argument str (metavar "FILE"))

opts :: ParserInfo Options
opts =
  info
    (parser <**> helper)
    ( fullDesc
        <> header "htreelox - a tree walker implementation of the lox language in haskell"
    )

cli :: IO ()
cli = do
  options <- execParser opts
  case options.file of
    Just f -> do
      runFile f
    Nothing -> do
      runPrompt emptyState
