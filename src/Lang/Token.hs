module Lang.Token
  ( Token (..)
  )
where

data Token
  = Integer Int
  | Floating Float
  | Identifier Text
  | OpenParen
  | CloseParen
  | Colon
  deriving stock (Eq, Show)
