module Lang.Repl
  ( repl
  )
where

import Lang.Evaluator (evaluate)
import Lang.Lexer (lex)
import Lang.Parser (parse)
import Lang.Printer (Print (..))
import Prelude hiding (print)
import System.Console.Repline
import Text.Pretty.Simple (pPrint)

import qualified Data.Text.IO as Text
import qualified System.IO as IO

repl :: MonadIO m => m ()
repl = liftIO $ evalReplOpts ReplOpts
  { banner = \_ -> pure "lang> "
  , command = evalCommand
  , options =
      [ ("lex", lexCommand)
      , ("parse", parseCommand)
      , ("eval", evalCommand)
      ]
  , prefix = Just ':'
  , multilineCommand = Nothing
  , tabComplete = File
  , initialiser = pure ()
  , finaliser = pure Exit
  }

lexCommand :: String -> HaskelineT IO ()
lexCommand source = handle $ lex (toText source)

parseCommand :: String -> HaskelineT IO ()
parseCommand source = handle $ lex (toText source) >>= parse

evalCommand :: String -> HaskelineT IO ()
evalCommand source = handle $ lex (toText source) >>= parse <&> evaluate

handle :: (Print a, Show a) => Either Text a -> HaskelineT IO ()
handle = \case
  Left err -> liftIO $ Text.hPutStrLn IO.stderr err
  Right value -> do
    putTextLn $ print value
    pPrint value
