module Ecru.Syntax
  ( Syntax (..)
  )
where

import Ecru.Literal (Literal (..))

data Syntax
  = Literal Literal
  | Variable Text
  | Lambda Text Syntax
  | Application Syntax Syntax
  deriving stock (Show)
