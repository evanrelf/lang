module Ecru.Eval
  ( eval
  )
where

import Ecru.Literal (Literal (..))
import Ecru.Syntax as Syntax (Syntax (..))
import Ecru.Value as Value (Value (..))

eval :: Syntax -> Value
eval = \case
  Syntax.Literal lit -> Value.Literal lit
  Syntax.Variable var -> Value.Variable var
  Syntax.Lambda param body -> Value.Lambda param (eval body)
  Syntax.Application fn arg -> apply fn arg

apply :: Syntax -> Syntax -> Value
apply = \cases
  -- Identity function
  (Syntax.Lambda param (Syntax.Variable var)) arg | param == var -> eval arg

  -- Const function
  (Syntax.Lambda param (Syntax.Lambda _ (Syntax.Variable var))) arg
    | param == var -> eval arg

  -- Add integers
  (Syntax.Application (Syntax.Variable "add") (Syntax.Literal (Integer x)))
    (Syntax.Literal (Integer y)) -> Value.Literal (Integer (x + y))

  -- Add floats
  (Syntax.Application (Syntax.Variable "add") (Syntax.Literal (Floating x)))
    (Syntax.Literal (Floating y)) -> Value.Literal (Floating (x + y))

  fn arg -> Value.Application (eval fn) (eval arg)
