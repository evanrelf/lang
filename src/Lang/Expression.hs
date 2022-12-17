module Lang.Expression
  ( Expression (..)
  )
where

data Expression
  = Variable Text
  | Application Expression Expression
  deriving stock (Show)
