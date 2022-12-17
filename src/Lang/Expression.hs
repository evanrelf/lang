module Lang.Expression
  ( Literal (..)
  , Expression (..)
  )
where

data Literal
  = Integer Int
  | Floating Float
  deriving stock (Show)

data Expression
  = Literal Literal
  | Variable Text
  | Application Expression Expression
  deriving stock (Show)
