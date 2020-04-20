module Expression.Parser where

import Text.Parsing.Parser.Expr as Expr
import Text.Parsing.Parser (ParserT, fail, ParseError, runParser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token (digit)
import Prelude
  (class Monad
  , (*>)
  , bind, pure, show, ($), ($>), (<$>), (<<<), (<>)
  )
import Data.String.CodeUnits as String
import Data.Array (many)
import Data.Maybe (Maybe(..))
import Data.Int as Int
import Data.Either (Either(..))
import Data.Show (class Show, show)
import Text.Parsing.Parser.String as Parser
import Text.Parsing.Parser.Combinators as Parser
import Data.List as List
import Data.Foldable (foldl)
import Data.Unit (Unit(..), unit)

import Expression.Term

type Parser m a = ParserT String m a


parseExpr :: String -> Either ParseError Expr
parseExpr s = runParser s exprParser


exprParser :: forall m. Monad m => Parser m Expr
exprParser = ?todo




lexChar :: forall m . Monad m => Char -> Parser m Unit
lexChar chr =
  do
    _ <- Parser.char chr
    _ <- Parser.whiteSpace
    pure unit

leftBracket :: forall m . Monad m => Parser m Unit
leftBracket = lexChar '('
  
rightBracket :: forall m . Monad m => Parser m Unit
rightBracket = lexChar ')'

bracketed :: forall m a . Monad m => Parser m a -> Parser m a
bracketed p =
  do
    _   <- Parser.try leftBracket
    res <- p
    _   <- rightBracket
    pure res


var :: Parser Expr
var = EVar <$> ident

ident :: Parser Text
ident = do
  (cons <$> lowerChar <*> takeWhileP Nothing (isAlphaNum))
    

