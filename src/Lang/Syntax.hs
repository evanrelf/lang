module Lang.Syntax
  ( Syntax (..)
  )
where

import Lang.Literal (Literal (..))

data Syntax
  = Literal Literal
  | Variable Text
  | Lambda Text Syntax
  | Application Syntax Syntax
  deriving stock (Show)
