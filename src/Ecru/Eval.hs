module Ecru.Eval
  ( eval
  )
where

import Ecru.Literal (Literal (..))
import Ecru.Syntax as Syntax (Syntax (..))
import Ecru.Value as Value (Value (..))

eval :: Syntax -> Value
eval = \case
  Syntax.Literal literal -> Value.Literal literal
  Syntax.Variable name -> Value.Variable name
  Syntax.Lambda parameter body -> Value.Lambda parameter (eval body)
  Syntax.Application function argument -> apply (eval function) (eval argument)

apply :: Value -> Value -> Value
apply = \cases
  -- Identity function
  (Value.Lambda param (Value.Variable var)) arg | param == var -> arg

  -- Const function
  (Value.Lambda param (Value.Lambda _ (Value.Variable var))) arg | param == var -> arg

  -- Add integers
  (Value.Application (Value.Variable "add") (Value.Literal (Integer x)))
    (Value.Literal (Integer y)) -> Value.Literal (Integer (x + y))

  -- Add floats
  (Value.Application (Value.Variable "add") (Value.Literal (Floating x)))
    (Value.Literal (Floating y)) -> Value.Literal (Floating (x + y))

  fn arg -> Value.Application fn arg
