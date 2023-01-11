module Lang.Printer
  ( Print (..)
  , print
  , Options (..)
  , defaultOptions
  )
where

import Lang.Expression as Expression (Expression (..))
import Lang.Expression as Literal (Literal (..))
import Lang.Token as Token (Token (..))
import Prelude hiding (print)

class Print a where
  printWithOptions :: Options -> a -> Text

print :: Print a => a -> Text
print = printWithOptions defaultOptions

instance Print Token where
  printWithOptions _ = \case
    Token.Identifier name -> name
    Token.Integer int -> show int
    Token.Floating float -> show float
    Token.OpenParen -> "("
    Token.CloseParen -> ")"

instance Print [Token] where
  printWithOptions options = unwords . fmap (printWithOptions options)

instance Print Literal where
  printWithOptions _ = \case
    Literal.Integer int -> show int
    Literal.Floating float -> show float

instance Print Expression where
  printWithOptions options@Options{extraParens} = \case
    Expression.Literal literal -> printWithOptions options literal
    Expression.Variable name -> name
    Expression.Application function argument | extraParens ->
      "(" <> printWithOptions options function <> " " <> printWithOptions options argument <> ")"
    Expression.Application function argument@(Expression.Application {}) ->
      printWithOptions options function <> " (" <> printWithOptions options argument <> ")"
    Expression.Application function argument ->
      printWithOptions options function <> " " <> printWithOptions options argument

data Options = Options
  { extraParens :: Bool
  }
  deriving stock (Generic, Show)

defaultOptions :: Options
defaultOptions = Options
  { extraParens = False
  }
