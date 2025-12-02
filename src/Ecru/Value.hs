module Ecru.Value
  ( Value (..)
  )
where

import Ecru.Literal (Literal (..))

data Value
  = Literal Literal
  | Variable Text
  | Lambda Text Value
  | Application Value Value
  deriving stock (Show)
