{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo #-}

module Ecru.Parse
  ( parse
  )
where

import Ecru.Literal as Literal (Literal (..))
import Ecru.Syntax (Syntax (..))
import Ecru.Token as Token (Token (..))
import Text.Earley qualified as E

parse :: [Token] -> Either Text Syntax
parse tokens =
  case E.fullParses (E.parser grammar) tokens of
    ([], report) -> Left $ show report
    (result : _, _) -> Right result

grammar :: E.Grammar r (E.Prod r Text Token Syntax)
grammar = mdo
  let rule name prod = E.rule (prod E.<?> name)

  let inParens prod = E.token OpenParen *> prod <* E.token CloseParen

  literal <- rule "literal" $ E.terminal \case
    Token.Integer int -> Just $ Literal (Literal.Integer int)
    Token.Floating float -> Just $ Literal (Literal.Floating float)
    _ -> Nothing

  identifier <- rule "identifier" $ E.terminal \case
    Identifier name -> Just name
    _ -> Nothing

  variable <- rule "variable" do
    Variable <$> identifier

  lambda <- rule "lambda" do
    Lambda <$> (identifier <* E.token Colon) <*> expression

  function <- rule "application_function" $ asum
    [ inParens lambda
    , inParens application
    , application
    , variable
    , literal
    , inParens argument
    ]

  argument <- rule "application_argument" $ asum
    [ inParens lambda
    , inParens application
    , variable
    , literal
    , inParens argument
    ]

  application <- rule "application" do
    Application <$> function <*> argument

  expression <- rule "expression" $ asum
    [ lambda
    , application
    , variable
    , literal
    , inParens expression
    ]

  pure expression
