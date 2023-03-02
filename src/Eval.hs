import LispVal

import qualified Data.Map as Map
import qualified Data.Text as T

funcEnv :: Map.Map T.Text LispVal
funcEnv = Map.fromList $ primEnv
          <> [("read",  Fun $ IFunc $ unop readFn),
              ("parse", Fun $ IFunc $ unop parseFn),
              ("eval",  Fun $ IFunc $ unop eval),
              ("show",  Fun $ IFunc $ unop (return . String . showVal))]

basicEnv :: EnvCtx
basicEnv = EnvCtx Map.empty funcEnv

readFn :: LispVal -> Eval LispVal
readFn (String txt) = lineToEvalForm txt
readFn val          = throw $ TypeMismatch "read expects str, got: " val

parseFn :: LispVal -> Eval LispVal
parseFn (String txt) = either (throw . PError . show) return $ readExpr txt
parseFn val = throw $ TypeMismatch "parse expects str, got: " val

-- exec while catching exceptions
-- unwraps caught exception to LispException
safeExec :: IO a -> IO (Either String a)
safeExec m = do
  result <- Control.Exception.try m
  case result of
    Left (eTop :: SomeException) ->
      case fromException eTop of
        Just (enclosed :: LispException) -> return $ Left (show enclosed)
        Nothing                          -> return $ Left (show eTop)
    Right val -> return $ Right val

-- run a program file
evalFile :: T.Text -> IO ()
evalFile fileExpr = (runASTinEnv basicEnv $ fileToEvalForm fileExpr)
                    >>= print

-- parse and (throw or evaluate)
fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (throw . PError . show)
                              evalBody
                              $ readExprFile input

runParseTest :: T.Text -> T.Text
runParseTest input = either (T.pack . show)
                            (T.pack . show)

sTDLIB :: T.Text
sTDLIB = "lib/stdlib.scm"

endOfList :: LispVal -> LispVal -> LispVal
endOfList (List x) expr = List $ x ++ [epr]
endOfList n _ = throw $ TypeMismatch "failure to get variable: " n

parseWithLib :: T.Text -> T.Text -> Either ParseError LispVal
parseWithLib std inp = do
  stdlib <- readExprFile std
  expr   <- readExpr inp
  return $ endOfList stdlib expr

getFileContents :: FilePath -> IO T.Text
getFileContents fname = do
  exists <- doesFileExist fname
  if exists
  then TIO.readFile fname
  else return "file does not exist"

textToEvalForm :: T.Text -> T.Text -> Eval LispVal
textToEvalForm std input = either (throw . PError . show) evalBody $ parseWithLib std input

evalText :: T.Text -> IO () -- REPL
evalText textExpr = do
  stdlib <- getFileContents $ T.unpack sTDLIB
  res <- runASTinEnv basicEnv $ textToEvalForm stdlib textExpr

-- unwrap Eval to access the data and run in the environment
runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runResourceT
                          $ runReaderT (unEval action) code

eval :: LispVal -> Eval LispVal

-- these are all special forms and need special evaluation logic
eval (List [Atom "quote", val]) = return val -- quote

eval (Number i) = return $ Number i -- autoquote
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval (List [])  = return Nil -- '() is nil
eval Nil        = return Nil

eval (List [Atom "write", rest]) = -- write
           return . String . T.pack $ show rest
eval (List ((:) (Atom "write") rest)) =
           return . String . T.pack . show $ List rest

eval n@(Atom _) = getVar n

getVar :: LispVal -> Eval LispVal -- variable lookup in EnvCtx
getVar (Atom atom) = do
  env <- ask
  case Map.lookup atom env of
        Just x  -> return x
        Nothing -> throw $ UnboundVar atom

eval (List [Atom "if", pred, truExpr, flsExpr]) = do -- if
  ifRes <- eval pred
  case ifRes of
    (Bool True)  -> eval truExpr
    (Bool False) -> eval flsExpr
                 -> throw $ BadSpecialForm "if"

eval (List [Atom "let", List pairs, expr]) = do -- let
  -- odds are atoms, evens are sexprs
  env   <- ask
  atoms <- mapM ensureAtom $ getEven pairs
  vals  <- mapM eval       $ getOdd  pairs
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals) <> env
  in local (const env') $ evalBody expr

getEven :: [t] -> [t]
getEven [] = []
getEven (x:xs) = x : getOdd xs

getOdd :: [t] -> [t]
getOdd [] = []
getOdd (x:xs) = getOdd xs

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n = throws $ TypeMismatch "atom" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom

eval (List [Atom "begin", rest]) = evalBody rest -- begin
eval (List ((:) (Atom "begin") rest)) = evalBody $ List rest

eval (List [Atom "define", varExpr, expr]) = do -- define
  varAtom <- ensureAtom varExpr
  evalVal <- eval expr
  env     <- ask
  let envFn = const $ Map.insert (extractVar varAtom) evalVal env
  in local envFn $ return varExpr

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do -- define
  evalVal <- eval defExpr
  env     <- ask
  let envFn = const $ Map.insert car evalVal env
  in local envFn $ evalBody $ List rest
evalBody x = eval x

eval (List [Atom "lambda", List params, expr]) = do -- lambda
  envLocal <- ask
  return $ Lambda (IFunc $ applyLambda expr params) env Local
eval (List (Atom "lambda":_)) = throw $ BadSpecialForm "lambda"

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
  env <- ask
  argEval <- mapM eval args
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a,b)) params argEval) <> env
  in local (const env') $ eval expr

eval (List ((:) x xs)) = do -- application, straight from λ calculus
  funVar <- eval x
  xVal   <- mapM eval xs
  case funVar of
    (Fun (IFunc internalFn)) -> internalFn xVal
    (Lambda (IFunc internalfn) boundenv) -> local (const boundenv)
                                                $ internalfn xVal
    _ -> throw $ NotFunction funVar

-- we need to support compositions of car and cdr like caddar
eval all@(List [Atom "cdr", List [Atom "quote", List (x:xs)]]) =
  return $ List xs
eval all@(List [Atom "cdr", arg@(List (x:xs))]) =
  case x of
    Atom _ -> do val <- eval arg
                 eval $ List [Atom "cdr", val]
    _ -> return $ List xs

eval all@(List [Atom "car", List [Atom "quote", List (x:xs)]]) =
  return $ x
eval all@(List [Atom "car", arg@(List (x:xs))]) =
  case x of
    Atom _ -> do val <- eval arg
                 eval $ List [Atom "car", val]
    _ -> return $ x
