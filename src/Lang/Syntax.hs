module Lang.Syntax
  ( Syntax (..)
  )
where

import Lang.Literal (Literal (..))

data Syntax
  = Literal Literal
  | Variable Text
  | Application Syntax Syntax
  deriving stock (Show)
