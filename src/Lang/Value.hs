module Lang.Value
  ( Value (..)
  )
where

import Lang.Literal (Literal (..))

data Value
  = Literal Literal
  | Variable Text
  | Application Value Value
  deriving stock (Show)
