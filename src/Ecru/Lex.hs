module Ecru.Lex
  ( Token (..)
  , lex
  )
where

import Data.Char qualified as Char
import Data.Text qualified as Text
import Ecru.Token (Token (..))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L

type Lexer = M.Parsec Void Text

lex :: Text -> Either Text [Token]
lex source =
  case M.runParser tokens "" source of
    Left err -> Left $ toText (M.errorBundlePretty err)
    Right ts -> pure ts

tokens :: Lexer [Token]
tokens = space *> M.manyTill token M.eof

token :: Lexer Token
token = asum
  [ Floating <$> M.try floating
  , Integer <$> integer
  , Identifier <$> identifier
  , OpenParen <$ symbol "("
  , CloseParen <$ symbol ")"
  , Colon <$ symbol ":"
  ]

identifier :: Lexer Text
identifier = lexeme do
  c <-
    M.satisfy \char -> or
      [ Char.isAsciiLower char
      , Char.isAsciiUpper char
      , char == '_'
      ]

  cs <-
    M.takeWhileP Nothing \char -> or
      [ Char.isAsciiLower char
      , Char.isAsciiUpper char
      , Char.isDigit char
      , char == '_'
      ]

  pure (c `Text.cons` cs)

integer :: Lexer Integer
integer = lexeme $ L.signed mempty L.decimal

floating :: Lexer Double
floating = lexeme $ L.signed mempty L.float

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme space

symbol :: Text -> Lexer Text
symbol = L.symbol space

space :: Lexer ()
space = L.space M.space1 (L.skipLineComment "#") empty
