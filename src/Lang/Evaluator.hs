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
  Syntax.Lambda parameter body ->
    Value.Lambda parameter (evaluate body)
  -- Identity function
  Syntax.Application
    (Syntax.Lambda parameter (Syntax.Variable name))
    argument | parameter == name -> evaluate argument
  -- Add integers
  Syntax.Application
    (Syntax.Application
      (Syntax.Variable "add")
      (Syntax.Literal (Integer x)))
    (Syntax.Literal (Integer y)) -> Value.Literal (Integer (x + y))
  -- Add floats
  Syntax.Application
    (Syntax.Application
      (Syntax.Variable "add")
      (Syntax.Literal (Floating x)))
    (Syntax.Literal (Floating y)) -> Value.Literal (Floating (x + y))
  Syntax.Application function argument ->
    Value.Application (evaluate function) (evaluate argument)
