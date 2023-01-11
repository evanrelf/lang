module Lang.Evaluator
  ( evaluate
  )
where

import Lang.Literal (Literal (..))
import Lang.Syntax as Syntax (Syntax (..))
import Lang.Value as Value (Value (..))

evaluate :: Syntax -> Value
evaluate = \case
  Syntax.Literal literal -> Value.Literal literal
  Syntax.Variable name -> Value.Variable name
  Syntax.Application
    (Syntax.Application
      (Syntax.Variable "add")
      (Syntax.Literal (Integer x)))
    (Syntax.Literal (Integer y)) -> Value.Literal (Integer (x + y))
  Syntax.Application
    (Syntax.Application
      (Syntax.Variable "add")
      (Syntax.Literal (Floating x)))
    (Syntax.Literal (Floating y)) -> Value.Literal (Floating (x + y))
  Syntax.Application function argument ->
    Value.Application (evaluate function) (evaluate argument)
