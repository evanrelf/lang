module Lang.Token
  ( Token (..)
  )
where

data Token
  = Identifier Text
  | Integer Int
  | Floating Float
  | OpenParen
  | CloseParen
  deriving stock (Eq, Show)
