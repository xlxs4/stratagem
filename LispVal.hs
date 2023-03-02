{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.Reader

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO A }
  deriving ( Monad
           , Functor
           , Applicative
           , MonadReader EnvCtx
           , MonadIO)

data LispVal
  = Atom T.text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool deriving (Eq)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }
