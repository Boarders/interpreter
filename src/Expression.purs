module Expression where

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



data Expr =
    Add Expr Expr
  | Lit Int

data Op =
  OpAdd

instance showExpr :: Show Expr where
  show = printExpr

interpretOp :: Op -> Expr -> Expr -> Expr
interpretOp OpAdd = Add



printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add exp1 exp2) = (printExpr exp1) <> " + " <> (printExpr exp2)

parseExpr :: String -> Either ParseError Expr
parseExpr s = runParser s exprParser


numParser :: forall m . Monad m => ParserT String m Int
numParser =
  do
    ds <- many digit
    
    let res = Int.fromString <<< String.fromCharArray $ ds
    case res of
      Just n -> pure n
      Nothing -> fail "Unable to parse number!"

litParser :: forall m . Monad m => ParserT String m Expr
litParser =
  Lit <$> numParser


opParser :: forall m . Monad m => ParserT String m Op
opParser = string "+" $> OpAdd

type OpList = {op :: Op, lit :: Expr}

opLitParser :: forall m . Monad m => ParserT String m OpList
opLitParser =
  do
    op <- opParser
    _  <- Parser.whiteSpace
    lit <- litParser
    _  <- Parser.whiteSpace
    pure $ { op: op, lit: lit}

    
exprParser :: forall m . Monad m => ParserT String m Expr
exprParser =
  do
    l <- litParser
    _ <- Parser.whiteSpace
    rest <- many opLitParser
    let result = foldl (\acc {op: o, lit: l} -> (interpretOp o) acc l) l rest
    pure result



    

