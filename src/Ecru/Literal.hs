module Ecru.Literal
  ( Literal (..)
  )
where

data Literal
  = Integer Integer
  | Floating Double
  deriving stock (Show)
