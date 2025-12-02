module Ecru.Token
  ( Token (..)
  )
where

import Data.Data (Data)

data Token
  = Integer Integer
  | Floating Double
  | Identifier Text
  | OpenParen
  | CloseParen
  | Colon
  deriving stock (Data, Eq, Show)
