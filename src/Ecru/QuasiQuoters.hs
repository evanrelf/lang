{-# LANGUAGE TemplateHaskellQuotes #-}

module Ecru.QuasiQuoters
  ( lexed
  , parsed
  )
where

import Data.Data (Data)
import Data.Text qualified as Text
import Data.Typeable qualified as Typeable
import Ecru.Lex (lex)
import Ecru.Parse (parse)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax qualified as TH

lexed :: QuasiQuoter
lexed = qqFrom lex

parsed :: QuasiQuoter
parsed = qqFrom (lex >=> parse)

qqFrom :: Data a => (Text -> Either Text a) -> QuasiQuoter
qqFrom parser = QuasiQuoter{ quoteExp, quotePat, quoteType, quoteDec }
  where
    quoteExp string =
      case parser (toText string) of
        Left err -> fail (toString err)
        Right x -> liftDataWithText x

    quotePat = unsupported "pattern"

    quoteType = unsupported "type"

    quoteDec = unsupported "declaration"

    unsupported context _ =
      fail . toString . unwords $
        [ "Unsupported operation: this QuasiQuoter cannot be used in a "
        , context <> "context"
        ]

    -- https://stackoverflow.com/q/38143464
    liftDataWithText :: Data a => a -> TH.Q TH.Exp
    liftDataWithText = TH.dataToExpQ \x -> liftText <$> Typeable.cast x

    liftText :: Text -> TH.Q TH.Exp
    liftText text = TH.AppE (TH.VarE 'Text.pack) <$> TH.lift (toString text)
