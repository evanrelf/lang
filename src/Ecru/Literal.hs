module Ecru.Literal
  ( Literal (..)
  )
where

import Data.Data (Data)

data Literal
  = Integer Integer
  | Floating Double
  deriving stock (Data, Show)
