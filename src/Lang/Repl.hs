module Lang.Repl
  ( repl
  )
where

import Lang.Lexer (lex)
import Lang.Parser (parse)
import System.Console.Repline
import Text.Pretty.Simple (pPrint)

import qualified Data.Text.IO as Text
import qualified System.IO as IO

repl :: MonadIO m => m ()
repl = liftIO $ evalReplOpts ReplOpts
  { banner = \_ -> pure "lang> "
  , command = parseCommand
  , options =
      [ ("lex", lexCommand)
      , ("parse", parseCommand)
      ]
  , prefix = Just ':'
  , multilineCommand = Nothing
  , tabComplete = File
  , initialiser = pure ()
  , finaliser = pure Exit
  }

lexCommand :: String -> HaskelineT IO ()
lexCommand source =
  lex (toText source)
  `dischargeError` \tokens -> pPrint tokens

parseCommand :: String -> HaskelineT IO ()
parseCommand source = do
  lex (toText source) >>= parse
  `dischargeError` \expression -> pPrint expression

dischargeError :: MonadIO m => Either Text a -> (a -> m ()) -> m ()
dischargeError e k =
  case e of
    Left err -> liftIO $ Text.hPutStrLn IO.stderr err
    Right x -> k x

infixr 0 `dischargeError`
