module Lang.Token
  ( Token (..)
  )
where

data Token
  = Integer Integer
  | Floating Double
  | Identifier Text
  | OpenParen
  | CloseParen
  | Colon
  deriving stock (Eq, Show)
