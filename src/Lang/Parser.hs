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

  variableProd <- rule "variable" $ E.terminal \case
    Identifier name -> Just $ Variable name
    _ -> Nothing

  argumentProd <- rule "argument" $ asum
    [ inParens applicationProd
    , variableProd
    , literalProd
    , inParens argumentProd
    ]

  applicationProd <- rule "application" do
    Application <$> expressionProd <*> argumentProd

  expressionProd <- rule "expression" $ asum
    [ applicationProd
    , variableProd
    , literalProd
    , inParens expressionProd
    ]

  pure expressionProd
