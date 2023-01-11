module Lang.Literal
  ( Literal (..)
  )
where

data Literal
  = Integer Int
  | Floating Float
  deriving stock (Show)
