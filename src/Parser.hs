module Parser (
  readExpr,
  readExprFile
) where

import LispVal ( LispVal(List, Bool, Nil, Number, String, Atom) )

import Text.Parsec
    ( char,
      digit,
      hexDigit,
      letter,
      octDigit,
      oneOf,
      string,
      eof,
      many1,
      sepBy,
      (<?>),
      (<|>),
      parse,
      try,
      ParseError,
      SourceName )
import Text.Parsec.Text ( Parser )

-- Alex & Happy would be probably better for this
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.List ( foldl' )

import qualified Data.Text as T
import Data.Char (digitToInt)
import Control.Monad (mzero)

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

-- lexeme specification
style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef {
  Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.commentLine = "--"
  , Tok.opStart = Tok.opLetter style
  , Tok.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
  , Tok.identStart = letter <|> oneOf "-+/*=|&><"
  , Tok.identLetter = digit <|> letter <|> oneOf "?+=|&-/"
  , Tok.reservedOpNames = ["'", "\""]
  }

-- pattern binding using record destructuring
Tok.TokenParser { Tok.parens = m_parens
                , Tok.identifier = m_identifier } = Tok.makeTokenParser style

-- lex input for reserved operators
reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer $ T.unpack op

-- parser functions; consume text, return LispVal data constructor
-- with the bound value we want using monadic binding
parseAtom :: Parser LispVal
parseAtom = do
  p <- m_identifier
  return $ Atom $ T.pack p

parseText :: Parser LispVal
parseText = do
  reservedOp "\""
  p <- many1 $ noneOf "\""
  reservedOp "\""
  return $ String . T.pack $ p

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseNegNum :: Parser LispVal
parseNegNum = do
  char '-'
  d <- many1 digit
  return $ Number . negate . read $ d

-- programs are a list of newline-delimited sexprs
parseList :: Parser LispVal
parseList = List . concat <$> Text.Parsec.many parseExpr
                                `sepBy` (char ' ' <|> char '\n')

parseSExp = List . concat <$> m_parens (Text.Parsec.many parseExpr
                                        `sepBy` (char ' ' <|> char '\n'))

parseQuote :: Parser LispVal
parseQuote = do
  reservedOp "\'"
  x <- parseExpr
  return $ List [Atom "quote", x]

parseReserved :: Parser LispVal
parseReserved = do
  reservedOp "Nil" >> return Nil
  <|> (reservedOp "#t" >> return (Bool True))
  <|> (reservedOp "#f" >> return (Bool False))

-- choose the first parser that can parse into a LispVal
parseExpr :: Parser LispVal
parseExpr = parseReserved <|> parseNumber
  <|> try parseNegNum
  <|> parseAtom
  <|> parseText
  <|> parseQuote
  <|> parseSExp

-- wrapper monad for IO; allows leading whitespace and EOF
contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

-- for the REPL
readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

-- for source
readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseList) "<file>"
