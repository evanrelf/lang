module Ecru.Value
  ( Value (..)
  )
where

import Ecru.Literal (Literal (..))
import Ecru.Syntax (Syntax (..))

data Value
  = Literal Literal
  | Variable Text
  | Lambda Text Syntax
  | Application Value Value
  deriving stock (Show)
