module Ecru.Literal
  ( Literal (..)
  )
where

import Data.Data (Data)

data Literal
  = Integer Integer
  | Floating Double
  | Boolean Bool
  deriving stock (Data, Show)
