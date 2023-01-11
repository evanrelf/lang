module Lang.Repl
  ( repl
  , replWithOptions
  , Options (..)
  , defaultOptions
  )
where

import Lang.Evaluator (evaluate)
import Lang.Lexer (lex)
import Lang.Parser (parse)
import Lang.Printer (Print (..))
import Optics ((%), set)
import Prelude hiding (print)
import System.Console.Repline hiding (Options)
import Text.Pretty.Simple (pPrint)

import qualified Data.String as String
import qualified Data.Text.IO as Text
import qualified Lang.Printer as Printer
import qualified System.IO as IO

repl :: MonadIO m => m ()
repl = replWithOptions defaultOptions

replWithOptions :: MonadIO m => Options -> m ()
replWithOptions options = liftIO do
  optionsIORef <- newIORef options

  evalReplOpts ReplOpts
    { banner = \_ -> pure "lang> "
    , command = evalCommand optionsIORef
    , options =
        [ ("lex", lexCommand optionsIORef)
        , ("parse", parseCommand optionsIORef)
        , ("eval", evalCommand optionsIORef)
        , ("set", setCommand optionsIORef)
        ]
    , prefix = Just ':'
    , multilineCommand = Nothing
    , tabComplete = File
    , initialiser = pure ()
    , finaliser = pure Exit
    }

lexCommand :: IORef Options -> String -> HaskelineT IO ()
lexCommand optionsIORef source = handle optionsIORef $
  lex (toText source)

parseCommand :: IORef Options -> String -> HaskelineT IO ()
parseCommand optionsIORef source = handle optionsIORef $
  lex (toText source) >>= parse

evalCommand :: IORef Options -> String -> HaskelineT IO ()
evalCommand optionsIORef source = handle optionsIORef $
  lex (toText source) >>= parse <&> evaluate

handle
  :: (Print a, Show a)
  => IORef Options
  -> Either Text a
  -> HaskelineT IO ()
handle optionsIORef result = do
  Options{printer} <- readIORef optionsIORef
  case result of
    Left err -> liftIO $ Text.hPutStrLn IO.stderr err
    Right value -> do
      putTextLn $ printWithOptions printer value
      pPrint value

setCommand :: IORef Options -> String -> HaskelineT IO ()
setCommand optionsIORef arguments = do
  case String.words arguments of
    ["printer.extraParens", readMaybe @Bool -> Just value] ->
      modifyIORef' optionsIORef $ set (#printer % #extraParens) value
    _ -> liftIO $ Text.hPutStrLn IO.stderr "Invalid option or argument(s)"

data Options = Options
  { printer :: Printer.Options
  }
  deriving stock (Generic, Show)

defaultOptions :: Options
defaultOptions = Options
  { printer = Printer.defaultOptions
  }
