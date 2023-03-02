import LispVal
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr

-- Alex & Happy would be probably better for this
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Data.Functor.Identity (Identity)

import qualified Data.Text as T
import Control.Applicative hiding ((<|>))

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
