module Lang.Lexer
  ( Token (..)
  , lex
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

data Token
  = Identifier Text
  | Integer Int
  | Floating Float
  | OpenParen
  | CloseParen
  deriving stock (Eq, Show)

type Parser = M.Parsec Void Text

lex :: Text -> Either Text [Token]
lex source =
  case M.runParser tokensParser "" source of
    Left err -> Left $ toText (M.errorBundlePretty err)
    Right tokens -> pure tokens

tokensParser :: Parser [Token]
tokensParser = spaceParser *> M.manyTill tokenParser M.eof

tokenParser :: Parser Token
tokenParser = asum
  [ Identifier <$> identifierParser
  , Floating <$> M.try floatingParser
  , Integer <$> integerParser
  , OpenParen <$ symbolParser "("
  , CloseParen <$ symbolParser ")"
  ]

identifierParser :: Parser Text
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

integerParser :: Parser Int
integerParser = lexemeParser $ L.signed mempty L.decimal

floatingParser :: Parser Float
floatingParser = lexemeParser $ L.signed mempty L.float

lexemeParser :: Parser a -> Parser a
lexemeParser = L.lexeme spaceParser

symbolParser :: Text -> Parser Text
symbolParser = L.symbol spaceParser

spaceParser :: Parser ()
spaceParser = L.space M.space1 (L.skipLineComment "#") empty
