module Ecru.Eval
  ( eval
  , prelude
  )
where

import Data.Map.Strict qualified as Map
import Ecru.Lex (lex)
import Ecru.Literal (Literal (..))
import Ecru.Parse (parse)
import Ecru.Syntax as Syntax (Syntax (..))
import Ecru.Value as Value (Value (..))

eval :: Map Text Value -> Syntax -> Value
eval scope = \case
  Syntax.Literal lit -> Value.Literal lit
  Syntax.Variable var ->
    case Map.lookup var scope of
      Just val -> val
      Nothing -> Value.Variable var
  Syntax.Lambda param body -> Value.Lambda param body
  Syntax.Application fn arg -> apply scope (eval scope fn) (eval scope arg)

apply :: Map Text Value -> Value -> Value -> Value
apply scope = \cases
  (Value.Lambda param body) arg ->
    eval (Map.insert param arg scope) body

  -- Add integers
  (Value.Application (Value.Variable "add") (Value.Literal (Integer x)))
    (Value.Literal (Integer y)) -> Value.Literal (Integer (x + y))

  -- Add floats
  (Value.Application (Value.Variable "add") (Value.Literal (Floating x)))
    (Value.Literal (Floating y)) -> Value.Literal (Floating (x + y))

  fn arg -> Value.Application fn arg

prelude :: Map Text Value
prelude = Map.fromList
  [ ("identity", expr "x: x")
  , ("const", expr "x: _: x")
  ]
  where
  expr :: Text -> Value
  expr = eval prelude . boldly parse . boldly lex

  boldly :: (a -> Either Text b) -> (a -> b)
  boldly f = either error identity . f
