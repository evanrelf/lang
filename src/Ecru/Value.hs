module Ecru.Value
  ( Value (..)
  )
where

import Ecru.Literal (Literal (..))
import Ecru.Syntax (Syntax (..))

data Value
  = Literal Literal
  | Variable Text
  | Lambda (Map Text Value) Text Syntax
  | Application Value Value
  deriving stock (Show)
