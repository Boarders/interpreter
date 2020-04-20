module Expression.Term where

import Prelude
  (class Monad
  , (*>)
  , bind
  , pure
  , show
  , ($)
  , ($>)
  , (<$>)
  , (<<<)
  , (<>)
  , map
  )
import Data.String.CodeUnits as String
import Data.Array (many)
import Data.Maybe (Maybe(..))
import Data.Int as Int
import Data.Either (Either(..))
import Data.Show (class Show, show)
import Data.List as List
import Data.Foldable (foldl, fold)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad (class Monad)


repl :: String -> String
repl str = str

data Atom =
    ANum   Int

instance showAtom :: Show Atom where
  show (ANum n) = show n

type Id = String

data Expr =
    EAtom Atom
  | EArray (Array Expr)
  | EVar Id
  | EApp Expr Expr
  | EFun Id Expr


instance showExpr :: Show Expr where
  show = printExpr

data EvalError =
  EvalError

instance showEvalError :: Show EvalError where
  show _ = "evaluation error: mismatch"


printExpr :: Expr -> String
printExpr (EAtom (ANum n)) = show n
printExpr (EArray exp) = show (map show exp)
printExpr (EVar id) = show id
printExpr (EApp exp1 exp2) =
  fold
  [ "("
  , show exp1
  , " "
  , show exp2
  , ")"
  ]
printExpr (EFun id body) =
  fold
  [ show id
  , " => "
  , show body
  ]
