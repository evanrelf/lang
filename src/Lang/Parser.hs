{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo #-}

module Lang.Parser
  ( parse
  )
where

import Lang.Expression (Expression (..))
import Lang.Lexer (Token (..))

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

  identifierProd <- rule "identifier" $ E.terminal \case
    Identifier name -> Just name
    _ -> Nothing

  variableProd <- rule "variable" do
    Variable <$> identifierProd

  applicationProd <- rule "application" do
    function <- asum
      [ inParens applicationProd
      , applicationProd
      , variableProd
      ]

    argument <- asum
      [ inParens applicationProd
      , variableProd
      ]

    pure $ Application function argument

  expressionProd <- rule "expression" $ asum
    [ variableProd
    , applicationProd
    , inParens expressionProd
    ]

  pure expressionProd
