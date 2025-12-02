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
  Syntax.Application fn arg -> apply scope fn arg

apply :: Map Text Value -> Syntax -> Syntax -> Value
apply scope = \cases
  (Syntax.Lambda param body) arg ->
    eval (fix \final -> Map.insert param (eval final arg) scope) body

  -- Add integers
  (Syntax.Application (Syntax.Variable "add") (Syntax.Literal (Integer x)))
    (Syntax.Literal (Integer y)) -> Value.Literal (Integer (x + y))

  -- Add floats
  (Syntax.Application (Syntax.Variable "add") (Syntax.Literal (Floating x)))
    (Syntax.Literal (Floating y)) -> Value.Literal (Floating (x + y))

  fn arg -> Value.Application (eval scope fn) (eval scope arg)

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
