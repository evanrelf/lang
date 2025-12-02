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
  case M.runParser tokensParser "" source of
    Left err -> Left $ toText (M.errorBundlePretty err)
    Right tokens -> pure tokens

tokensParser :: Lexer [Token]
tokensParser = spaceParser *> M.manyTill tokenParser M.eof

tokenParser :: Lexer Token
tokenParser = asum
  [ Floating <$> M.try floatingParser
  , Integer <$> integerParser
  , Identifier <$> identifierParser
  , OpenParen <$ symbolParser "("
  , CloseParen <$ symbolParser ")"
  , Colon <$ symbolParser ":"
  ]

identifierParser :: Lexer Text
identifierParser = lexemeParser do
  c <-
    M.satisfy \char -> or
      [ Char.isAsciiLower char
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

integerParser :: Lexer Integer
integerParser = lexemeParser $ L.signed mempty L.decimal

floatingParser :: Lexer Double
floatingParser = lexemeParser $ L.signed mempty L.float

lexemeParser :: Lexer a -> Lexer a
lexemeParser = L.lexeme spaceParser

symbolParser :: Text -> Lexer Text
symbolParser = L.symbol spaceParser

spaceParser :: Lexer ()
spaceParser = L.space M.space1 (L.skipLineComment "#") empty
