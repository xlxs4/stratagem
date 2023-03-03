module Cli (
  cliIface
) where

import Eval ( evalFile ) -- evalFile :: T.Text -> IO ()
import Repl ( mainLoop ) -- Repl.mainLoop :: IO ()
import System.Directory ( doesFileExist )
import Data.Text.IO as TIO ( readFile, putStrLn )
import Options.Applicative
    ( helper,
      execParser,
      strOption,
      short,
      progDesc,
      metavar,
      long,
      info,
      help,
      header,
      fullDesc,
      flag',
      Alternative((<|>)),
      Parser )

runScript :: FilePath -> IO ()
runScript fname = do
  exists <- doesFileExist fname
  if exists
  then TIO.readFile fname >>= evalFile fname
  else TIO.putStrLn "file does not exist"

data LineOpts = UseReplLineOpts | RunScriptLineOpts String

parseLineOpts :: Parser LineOpts
parseLineOpts = runScriptOpt <|> runReplOpt
  where
    runScriptOpt =
      RunScriptLineOpts <$> strOption (long "script"
                                       <> short 's'
                                       <> metavar "SCRIPT"
                                       <> help "file containing the script you want to run")
    runReplOpt =
      UseReplLineOpts <$ flag' () (long "repl"
                                   <> short 'r'
                                   <> help "run as interactive read/evaluate/print/loop")

stratagemEntryPoint :: LineOpts -> IO ()
stratagemEntryPoint UseReplLineOpts = Repl.mainLoop -- repl
stratagemEntryPoint (RunScriptLineOpts script) = runScript script

cliIface :: IO ()
cliIface = execParser opts >>= stratagemEntryPoint
  where
    opts = info (helper <*> parseLineOpts)
      ( fullDesc
      <> header "executable binary for stratagem"
      <> progDesc "contains an entry point for both running scripts and repl")
