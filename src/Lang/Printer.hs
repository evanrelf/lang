module Lang.Printer
  ( Print (..)
  )
where

import Lang.Expression as Expression (Expression (..))
import Lang.Expression as Literal (Literal (..))
import Lang.Token as Token (Token (..))
import Prelude hiding (print)

class Print a where
  print :: a -> Text

instance Print Token where
  print = \case
    Token.Identifier name -> name
    Token.Integer int -> show int
    Token.Floating float -> show float
    Token.OpenParen -> "("
    Token.CloseParen -> ")"

instance Print [Token] where
  print = unwords . fmap print

instance Print Literal where
  print = \case
    Literal.Integer int -> show int
    Literal.Floating float -> show float

instance Print Expression where
  print = \case
    Expression.Literal literal -> print literal
    Expression.Variable name -> name
    Expression.Application function argument@(Expression.Application {}) ->
      print function <> " (" <> print argument <> ")"
    Expression.Application function argument ->
      print function <> " " <> print argument
