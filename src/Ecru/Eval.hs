{-# LANGUAGE QuasiQuotes #-}

module Ecru.Eval
  ( eval
  , prelude
  )
where

import Data.Map.Strict qualified as Map
import Ecru.Literal (Literal (..))
import Ecru.QuasiQuoters (parsed)
import Ecru.Syntax as Syntax (Syntax (..))
import Ecru.Value as Value (Value (..))

eval :: Map Text Syntax -> Syntax -> Value
eval scope = \case
  Syntax.Literal lit -> Value.Literal lit
  Syntax.Variable var -> Value.Variable var
  Syntax.Lambda param body -> Value.Lambda param (eval scope body)
  Syntax.Application fn arg -> apply scope fn arg

apply :: Map Text Syntax -> Syntax -> Syntax -> Value
apply scope = \cases
  -- Identity function
  (Syntax.Lambda param (Syntax.Variable var)) arg | param == var -> eval scope arg

  -- Const function
  (Syntax.Lambda param (Syntax.Lambda _ (Syntax.Variable var))) arg
    | param == var -> eval scope arg

  -- Add integers
  (Syntax.Application (Syntax.Variable "add") (Syntax.Literal (Integer x)))
    (Syntax.Literal (Integer y)) -> Value.Literal (Integer (x + y))

  -- Add floats
  (Syntax.Application (Syntax.Variable "add") (Syntax.Literal (Floating x)))
    (Syntax.Literal (Floating y)) -> Value.Literal (Floating (x + y))

  fn arg -> Value.Application (eval scope fn) (eval scope arg)

prelude :: Map Text Syntax
prelude = Map.fromList
  [ ("identity", [parsed|x: x|])
  , ("const", [parsed|x: _: x|])
  ]
