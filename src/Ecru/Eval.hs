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

eval :: Map Text Value -> Syntax -> Either Text Value
eval scope = \case
  Syntax.Literal lit -> Right $ Value.Literal lit
  Syntax.Variable var ->
    case Map.lookup var scope of
      Just val -> Right val
      Nothing -> Right $ Value.Variable var
  Syntax.Lambda param body -> Right $ Value.Lambda scope param body
  Syntax.Application fn arg -> do
    fn' <- eval scope fn
    arg' <- eval scope arg
    apply fn' arg'

apply :: Value -> Value -> Either Text Value
apply = \cases
  (Value.Lambda scope param body) arg ->
    eval (Map.insert param arg scope) body

  -- TODO: Check types of `t` and `f` match
  (Value.Application (Value.Application (Value.Variable "builtin/if") p) t) f ->
    case p of
      Value.Literal (Boolean p') -> Right $ if p' then t else f
      _ -> Left "error: arguments to `if` not valid"

  (Value.Application (Value.Variable "builtin/eq") x) y ->
    case (x, y) of
      (Value.Literal (Integer x'), Value.Literal (Integer y')) ->
        Right $ Value.Literal (Boolean (x' == y'))
      (Value.Literal (Floating x'), Value.Literal (Floating y')) ->
        Right $ Value.Literal (Boolean (x' == y'))
      (Value.Literal (Boolean x'), Value.Literal (Boolean y')) ->
        Right $ Value.Literal (Boolean (x' == y'))
      (Value.Literal _, Value.Literal _) ->
        Left "error: arguments to `eq` not the same type"
      _ ->
        Left "error: arguments to `eq` not valid"

  (Value.Application (Value.Variable "builtin/add") x) y ->
    case (x, y) of
      (Value.Literal (Integer x'), Value.Literal (Integer y')) ->
        Right $ Value.Literal (Integer (x' + y'))
      (Value.Literal (Floating x'), Value.Literal (Floating y')) ->
        Right $ Value.Literal (Floating (x' + y'))
      (Value.Literal _, Value.Literal _) ->
        Left "error: arguments to `add` not the same type"
      _ ->
        Left "error: arguments to `add` not valid"

  (Value.Application (Value.Variable "builtin/sub") x) y ->
    case (x, y) of
      (Value.Literal (Integer x'), Value.Literal (Integer y')) ->
        Right $ Value.Literal (Integer (x' - y'))
      (Value.Literal (Floating x'), Value.Literal (Floating y')) ->
        Right $ Value.Literal (Floating (x' - y'))
      (Value.Literal _, Value.Literal _) ->
        Left "error: arguments to `sub` not the same type"
      _ ->
        Left "error: arguments to `sub` not valid"

  (Value.Application (Value.Variable "builtin/mul") x) y ->
    case (x, y) of
      (Value.Literal (Integer x'), Value.Literal (Integer y')) ->
        Right $ Value.Literal (Integer (x' * y'))
      (Value.Literal (Floating x'), Value.Literal (Floating y')) ->
        Right $ Value.Literal (Floating (x' * y'))
      (Value.Literal _, Value.Literal _) ->
        Left "error: arguments to `mul` not the same type"
      _ ->
        Left "error: arguments to `mul` not valid"

  (Value.Application (Value.Variable "builtin/div") x) y ->
    case (x, y) of
      (Value.Literal (Integer x'), Value.Literal (Integer y')) ->
        Right $ Value.Literal (Integer (x' `div` y'))
      (Value.Literal (Floating x'), Value.Literal (Floating y')) ->
        Right $ Value.Literal (Floating (x' / y'))
      (Value.Literal _, Value.Literal _) ->
        Left "error: arguments to `div` not the same type"
      _ ->
        Left "error: arguments to `div` not valid"

  fn arg -> Right $ Value.Application fn arg

prelude :: Map Text Value
prelude = Map.fromList
  [ ("identity", expr "x: x")
  , ("const", expr "x: _: x")
  , ("flip", expr "f: x: y: f y x")
  , ("if", Value.Variable "builtin/if")
  , ("eq", Value.Variable "builtin/eq")
  , ("add", Value.Variable "builtin/add")
  , ("sub", Value.Variable "builtin/sub")
  , ("mul", Value.Variable "builtin/mul")
  , ("div", Value.Variable "builtin/div")
  , ("True", Value.Literal (Boolean True))
  , ("False", Value.Literal (Boolean False))
  -- TODO: Figure out recursion
  -- , ("ycombinator", expr "f: (x: f (x x)) (x: f (x x))")
  -- , ("factorial", expr "n: if (eq n 0) 1 (mul n (factorial (sub n 1)))")
  -- , ("factorial", expr "ycombinator (f: n: if (eq n 0) 1 (mul n (f (sub n 1))))")
  ]
  where
  expr :: Text -> Value
  expr = either error identity . (lex >=> parse >=> eval prelude)
