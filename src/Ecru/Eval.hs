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
  Syntax.Application function argument -> apply function argument

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
