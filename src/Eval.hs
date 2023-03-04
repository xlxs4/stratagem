{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval (
  evalText,
  evalFile,
  runParseTest,
  safeExec,
  -- testing
  runASTinEnv,
  basicEnv,
  fileToEvalForm,
  textToEvalForm,
  getFileContents
) where

import Prim ( primEnv, unop )
import Parser ( readExpr, readExprFile )
import LispVal
    ( LispException(Default, PError, UnboundVar, TypeMismatch,
               BadSpecialForm, NotFunction ),
    IFunc(IFunc),
    LispVal(..),
    Eval(unEval),
    EnvCtx(..),
    showVal )

import Data.Map as Map
    ( empty, fromList, insert, lookup, partition, toList, union, Map )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory ( doesFileExist )

import Text.Parsec ( ParseError )

import Control.Monad.Reader
    ( asks, MonadIO(liftIO), MonadReader(local, ask), ReaderT(runReaderT) )
import Control.Exception
    ( try, throw, Exception(fromException), SomeException )

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

-- unwrap Eval to access the data and run in the environment
runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runReaderT (unEval action) code

lineToEvalForm :: T.Text -> Eval LispVal
lineToEvalForm input = either (throw . PError . show) eval $ readExpr input

-- run a program file
evalFile :: FilePath -> T.Text -> IO ()
evalFile filePath fileExpr = runASTinEnv basicEnv (fileToEvalForm filePath fileExpr) >>= print

-- parse and (throw or evaluate)
fileToEvalForm :: FilePath -> T.Text -> Eval LispVal
fileToEvalForm filePath input = either (throw . PError . show) evalBody $ readExprFile filePath input

-- view AST
runParseTest :: T.Text -> T.Text
runParseTest input = either (T.pack . show) (T.pack . show) $ readExpr input

sTDLIB :: FilePath
sTDLIB = "lib/stdlib.scm"

endOfList :: LispVal -> LispVal -> LispVal
endOfList (List x) expr = List $ x ++ [expr]
endOfList n _ = throw $ TypeMismatch "failure to get variable: " n

parseWithLib :: T.Text -> T.Text -> Either ParseError LispVal
parseWithLib std inp = do
  stdlib <- readExprFile sTDLIB std
  expr   <- readExpr inp
  return $ endOfList stdlib expr

getFileContents :: FilePath -> IO T.Text
getFileContents fname = do
  exists <- doesFileExist fname
  if exists then TIO.readFile fname else return "file does not exist"

textToEvalForm :: T.Text -> T.Text -> Eval LispVal
textToEvalForm std input = either (throw . PError . show) evalBody $ parseWithLib std input

evalText :: T.Text -> IO () -- REPL
evalText textExpr = do
  stdlib <- getFileContents sTDLIB
  res <- runASTinEnv basicEnv $ textToEvalForm stdlib textExpr
  print res

 -- variable lookup in EnvCtx
getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
  EnvCtx{..} <- ask
  case Map.lookup atom (Map.union fenv env) of -- lookup, but prefer functions
    Just x  -> return x
    Nothing -> throw $ UnboundVar atom
getVar n = throw $ TypeMismatch "failure to get variable: " n

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n = throw $ TypeMismatch "expected an atomic value: " n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom
extractVar n = throw $ TypeMismatch "expected an atomic value: " n

getEven :: [t] -> [t]
getEven [] = []
getEven (x:xs) = x : getOdd xs

getOdd :: [t] -> [t]
getOdd [] = []
getOdd (_:xs) = getEven xs

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = bindArgsEval params args expr

bindArgsEval :: [LispVal] -> [LispVal] -> LispVal -> Eval LispVal
bindArgsEval params args expr = do
  EnvCtx{..} <- ask
  let newVars = zipWith (\a b -> (extractVar a,b)) params args
  let (newEnv, newFenv) = Map.partition (not . isLambda) $ Map.fromList newVars
  local (const $ EnvCtx (newEnv <> env) (newFenv <> fenv)) $ eval expr

isLambda :: LispVal -> Bool
isLambda (List ((Atom "lambda"):_)) = True
isLambda _ = False

eval :: LispVal -> Eval LispVal
eval (List [Atom "dumpEnv", x]) = do
  EnvCtx{..} <- ask
  liftIO $ print $ toList env
  liftIO $ print $ toList fenv
  eval x

eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval (List [])  = return Nil -- '() is Nil
eval Nil        = return Nil
eval n@(Atom _) = getVar n

eval (List [Atom "showSF", rest])      = return . String . T.pack $ show rest
eval (List ((:) (Atom "showSF") rest)) = return . String . T.pack . show $ List rest

-- these are all special forms and need special evaluation logic
eval (List [Atom "quote", val]) = return val -- quote

eval (List [Atom "if", pred, truExpr, flsExpr]) = do -- if
  ifRes <- eval pred
  case ifRes of
    (Bool True)  -> eval truExpr
    (Bool False) -> eval flsExpr
    _            -> throw $ BadSpecialForm "if first arg must eval into a bool"
eval (List ( (:) (Atom "if") _ )) = throw $ BadSpecialForm "(if <bool> <s-expr> <s-epr>)"

eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest)) = evalBody $ List rest

eval (List [Atom "define", varExpr, defExpr]) = do -- top-level define
  EnvCtx{} <- ask
  _varAtom <- ensureAtom varExpr
  _evalVal <- eval defExpr
  bindArgsEval [varExpr] [defExpr] varExpr

eval (List [Atom "let", List pairs, expr]) = do
  EnvCtx{} <- ask
  atoms <- mapM ensureAtom $ getEven pairs
  vals  <- mapM eval       $ getOdd  pairs
  bindArgsEval atoms vals expr
eval (List (Atom "let":_)) = throw $ BadSpecialForm "(let <pairs> <s-expr>)"

eval (List [Atom "lambda", List params, expr]) = do
  asks (Lambda (IFunc $ applyLambda expr params))
eval (List (Atom "lambda":_)) = throw $ BadSpecialForm "(lambda <params> <s-expr>)"

-- need to get combined forms of car and cdr
eval (List [Atom "cdr", List [Atom "quote", List (_:xs)]]) =
  return $ List xs
eval (List [Atom "cdr", arg@(List (x:xs))]) =
  case x of
    -- can the list be evaluated?
    Atom _ -> do val <- eval arg
                 eval $ List [Atom "cdr", val]
    _ -> return $ List xs

eval (List [Atom "car", List [Atom "quote", List (x:_)]]) =
  return x
eval (List [Atom "car", arg@(List (x:_))]) =
  case x of
    Atom _ -> do val <- eval arg
                 eval $ List [Atom "car", val]
    _ -> return x

eval (List ((:) x xs)) = do
  EnvCtx{..} <- ask
  funVar <- eval x
  xVal <- mapM eval xs
  case funVar of
    (Fun (IFunc internalFn)) -> internalFn xVal
    (Lambda (IFunc definedFn) (EnvCtx benv _bfenv)) -> local (const $ EnvCtx benv fenv) $ definedFn xVal
    _ -> throw $ NotFunction funVar

eval x = throw $ Default x -- fallthrough

updateEnv :: T.Text -> LispVal -> EnvCtx -> EnvCtx
updateEnv var e@(Fun _) EnvCtx{..} = EnvCtx env $ Map.insert var e fenv
updateEnv var e@(Lambda _ _) EnvCtx{..} = EnvCtx env $ Map.insert var e fenv
updateEnv var e EnvCtx{..} = EnvCtx (Map.insert var e env) fenv

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
  evalVal <- eval defExpr
  ctx <- ask
  local (const $ updateEnv var evalVal ctx) $ eval rest

evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
  evalVal <- eval defExpr
  ctx <- ask
  local (const $ updateEnv var evalVal ctx) $ evalBody $ List rest

evalBody x = eval x
