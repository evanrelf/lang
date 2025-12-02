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
  Syntax.Lambda parameter body ->
    Value.Lambda parameter (eval body)
  -- Identity function
  Syntax.Application
    (Syntax.Lambda parameter (Syntax.Variable name))
    argument | parameter == name -> eval argument
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
    Value.Application (eval function) (eval argument)
