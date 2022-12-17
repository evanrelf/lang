module Lang.Evaluator
  ( evaluate
  )
where

import Lang.Expression (Expression (..), Literal (..))

evaluate :: Expression -> Expression
evaluate = \case
  Literal literal -> Literal literal
  Variable name -> Variable name
  Application
    (Application
      (Variable "add")
      (Literal (Integer x)))
    (Literal (Integer y)) -> Literal (Integer (x + y))
  Application
    (Application
      (Variable "add")
      (Literal (Floating x)))
    (Literal (Floating y)) -> Literal (Floating (x + y))
  expression -> expression
