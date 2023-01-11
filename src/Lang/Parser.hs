{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo #-}

module Lang.Parser
  ( parse
  )
where

import Lang.Expression as Expression (Expression (..), Literal (..))
import Lang.Token as Token (Token (..))

import qualified Text.Earley as E

parse :: [Token] -> Either Text Expression
parse tokens =
  case E.fullParses (E.parser grammar) tokens of
    ([], report) -> Left $ show report
    (result : _, _) -> Right result

grammar :: E.Grammar r (E.Prod r Text Token Expression)
grammar = mdo
  let rule name prod = E.rule (prod E.<?> name)

  let inParens prod = E.token OpenParen *> prod <* E.token CloseParen

  literalProd <- rule "literal" $ E.terminal \case
    Token.Integer int -> Just $ Literal (Expression.Integer int)
    Token.Floating float -> Just $ Literal (Expression.Floating float)
    _ -> Nothing

  variableProd <- rule "variable" $ E.terminal \case
    Identifier name -> Just $ Variable name
    _ -> Nothing

  applicationProd <- rule "application" do
    function <- asum
      [ inParens applicationProd
      , applicationProd
      , variableProd
      ]

    argument <- asum
      [ inParens applicationProd
      , variableProd
      , literalProd
      ]

    pure $ Application function argument

  expressionProd <- rule "expression" $ asum
    [ variableProd
    , applicationProd
    , literalProd
    , inParens expressionProd
    ]

  pure expressionProd
