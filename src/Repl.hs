type Repl a = InputT IO a

mainLoop :: IO ()
mainLoop = runInputT defaultSettings repl

repl :: Repl ()
repl = do
  minput <- getInputLine "Repl> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> liftIO (process input) >> repl
    -- to display the AST, you can use this instead
    -- Just input -> (liftIO $ processToAst input) >> repl

process :: String -> IO ()
process str = do
  res <- safeExec $ evalText $ T.pack str
  either putStrLn return res

processToAST :: String -> IO ()
processToAST str = print $ runParseTest $ T.pack str
