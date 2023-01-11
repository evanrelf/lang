module Lang.Syntax
  ( Syntax (..)
  , Literal (..)
  )
where

data Syntax
  = Literal Literal
  | Variable Text
  | Application Syntax Syntax
  deriving stock (Show)

data Literal
  = Integer Int
  | Floating Float
  deriving stock (Show)
