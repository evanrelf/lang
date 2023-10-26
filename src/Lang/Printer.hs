module Lang.Printer
  ( Print (..)
  , print
  , Options (..)
  , defaultOptions
  )
where

import Lang.Literal as Literal (Literal (..))
import Lang.Syntax as Syntax (Syntax (..))
import Lang.Token as Token (Token (..))
import Lang.Value as Value (Value (..))
import Prelude hiding (print)

class Print a where
  printWithOptions :: Options -> a -> Text

print :: Print a => a -> Text
print = printWithOptions defaultOptions

instance Print Token where
  printWithOptions :: Options -> Token -> Text
  printWithOptions _ = \case
    Token.Identifier name -> name
    Token.Integer int -> show int
    Token.Floating float -> show float
    Token.OpenParen -> "("
    Token.CloseParen -> ")"
    Token.Colon -> ":"

instance Print [Token] where
  printWithOptions :: Options -> [Token] -> Text
  printWithOptions options = unwords . fmap (printWithOptions options)

instance Print Literal where
  printWithOptions :: Options -> Literal -> Text
  printWithOptions _ = \case
    Literal.Integer int -> show int
    Literal.Floating float -> show float

instance Print Syntax where
  printWithOptions :: Options -> Syntax -> Text
  printWithOptions options@Options{extraParens} = \case
    Syntax.Literal literal -> printWithOptions options literal
    Syntax.Variable name -> name
    Syntax.Lambda parameter body ->
      parameter <> ": " <> printWithOptions options body
    Syntax.Application function argument -> do
      let f = do
            let raw = printWithOptions options function
            case function of
              Syntax.Lambda {} -> "(" <> raw <> ")"
              _ -> raw
      let x = do
            let raw = printWithOptions options argument
            case argument of
              Syntax.Lambda {} -> "(" <> raw <> ")"
              Syntax.Application {} -> "(" <> raw <> ")"
              _ -> raw
      if extraParens
        then "(" <> f <> " " <> x <> ")"
        else f <> " " <> x

instance Print Value where
  printWithOptions :: Options -> Value -> Text
  printWithOptions options@Options{extraParens} = \case
    Value.Literal literal -> printWithOptions options literal
    Value.Variable name -> name
    Value.Lambda parameter body ->
      parameter <> ": " <> printWithOptions options body
    Value.Application function argument -> do
      let f = do
            let raw = printWithOptions options function
            case function of
              Value.Lambda {} -> "(" <> raw <> ")"
              _ -> raw
      let x = do
            let raw = printWithOptions options argument
            case argument of
              Value.Lambda {} -> "(" <> raw <> ")"
              Value.Application {} -> "(" <> raw <> ")"
              _ -> raw
      if extraParens
        then "(" <> f <> " " <> x <> ")"
        else f <> " " <> x

data Options = Options
  { extraParens :: Bool
  }
  deriving stock (Generic, Show)

defaultOptions :: Options
defaultOptions = Options
  { extraParens = False
  }
