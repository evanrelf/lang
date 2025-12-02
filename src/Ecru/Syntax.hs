module Ecru.Syntax
  ( Syntax (..)
  )
where

import Data.Data (Data)
import Ecru.Literal (Literal (..))

data Syntax
  = Literal Literal
  | Variable Text
  | Lambda Text Syntax
  | Application Syntax Syntax
  deriving stock (Data, Show)
