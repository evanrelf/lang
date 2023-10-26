module Lang.Literal
  ( Literal (..)
  )
where

data Literal
  = Integer Integer
  | Floating Double
  deriving stock (Show)
