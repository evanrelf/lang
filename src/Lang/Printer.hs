module Lang.Printer
  ( Print (..)
  , print
  , Options (..)
  , defaultOptions
  )
where

import Lang.Value as Value (Value (..))
import Lang.Literal as Literal (Literal (..))
import Lang.Syntax as Syntax (Syntax (..))
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

instance Print Syntax where
  printWithOptions options@Options{extraParens} = \case
    Syntax.Literal literal -> printWithOptions options literal
    Syntax.Variable name -> name
    Syntax.Application function argument | extraParens ->
      "(" <> printWithOptions options function <> " " <> printWithOptions options argument <> ")"
    Syntax.Application function argument@(Syntax.Application {}) ->
      printWithOptions options function <> " (" <> printWithOptions options argument <> ")"
    Syntax.Application function argument ->
      printWithOptions options function <> " " <> printWithOptions options argument

instance Print Value where
  printWithOptions options@Options{extraParens} = \case
    Value.Literal literal -> printWithOptions options literal
    Value.Variable name -> name
    Value.Application function argument | extraParens ->
      "(" <> printWithOptions options function <> " " <> printWithOptions options argument <> ")"
    Value.Application function argument@(Value.Application {}) ->
      printWithOptions options function <> " (" <> printWithOptions options argument <> ")"
    Value.Application function argument ->
      printWithOptions options function <> " " <> printWithOptions options argument

data Options = Options
  { extraParens :: Bool
  }
  deriving stock (Generic, Show)

defaultOptions :: Options
defaultOptions = Options
  { extraParens = False
  }
