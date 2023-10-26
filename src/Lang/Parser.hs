{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo #-}

module Lang.Parser
  ( parse
  )
where

import Lang.Literal as Literal (Literal (..))
import Lang.Syntax (Syntax (..))
import Lang.Token as Token (Token (..))
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

  literalProd <- rule "literal" $ E.terminal \case
    Token.Integer int -> Just $ Literal (Literal.Integer int)
    Token.Floating float -> Just $ Literal (Literal.Floating float)
    _ -> Nothing

  identifierProd <- rule "identifier" $ E.terminal \case
    Identifier name -> Just name
    _ -> Nothing

  variableProd <- rule "variable" do
    Variable <$> identifierProd

  lambdaProd <- rule "lambda" do
    Lambda <$> (identifierProd <* E.token Colon) <*> expressionProd

  functionProd <- rule "application_function" $ asum
    [ inParens lambdaProd
    , inParens applicationProd
    , applicationProd
    , variableProd
    , literalProd
    , inParens argumentProd
    ]

  argumentProd <- rule "application_argument" $ asum
    [ inParens lambdaProd
    , inParens applicationProd
    , variableProd
    , literalProd
    , inParens argumentProd
    ]

  applicationProd <- rule "application" do
    Application <$> functionProd <*> argumentProd

  expressionProd <- rule "expression" $ asum
    [ lambdaProd
    , applicationProd
    , variableProd
    , literalProd
    , inParens expressionProd
    ]

  pure expressionProd
