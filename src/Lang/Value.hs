module Lang.Value
  ( Value (..)
  )
where

import Lang.Literal (Literal (..))

data Value
  = Literal Literal
  | Variable Text
  | Lambda Text Value
  | Application Value Value
  deriving stock (Show)
