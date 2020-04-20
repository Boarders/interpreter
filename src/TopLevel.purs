module TopLevel where

import Prelude ((<>))
import Expression.Parser as Parser
import Expression.Term
import Data.Either (Either(..))
import Data.Show (class Show, show)


runExpr :: String -> String
runExpr str =
  case Parser.parseExpr str of
    Right res -> show res
    Left error -> "error!"
  
