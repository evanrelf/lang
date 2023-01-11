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
import Optics
import Prelude hiding (print)
import System.Console.Repline hiding (Options)
import Text.Pretty.Simple (pPrint)

import qualified Data.String as String
import qualified Data.Text as Text
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
        , ("help", helpCommand optionsIORef)
        ]
    , prefix = Just ':'
    , multilineCommand = Nothing
    , tabComplete = File
    , initialiser = pure ()
    , finaliser = pure Exit
    }

lexCommand :: IORef Options -> String -> HaskelineT IO ()
lexCommand optionsIORef source = do
  Options{printer} <- readIORef optionsIORef

  case lex (toText source) of
    Left err -> liftIO $ Text.hPutStrLn IO.stderr err
    Right value -> do
      putTextLn $ printWithOptions printer value
      pPrint value

parseCommand :: IORef Options -> String -> HaskelineT IO ()
parseCommand optionsIORef source = do
  Options{printer} <- readIORef optionsIORef

  case lex (toText source) >>= parse of
    Left err -> liftIO $ Text.hPutStrLn IO.stderr err
    Right value -> do
      putTextLn $ printWithOptions printer value
      pPrint value

evalCommand :: IORef Options -> String -> HaskelineT IO ()
evalCommand optionsIORef source = do
  Options{printer} <- readIORef optionsIORef

  case lex (toText source) >>= parse <&> evaluate of
    Left err -> liftIO $ Text.hPutStrLn IO.stderr err
    Right value -> putTextLn $ printWithOptions printer value

setCommand :: IORef Options -> String -> HaskelineT IO ()
setCommand optionsIORef arguments = do
  case String.words arguments of
    ["printer.extraParens", readMaybe @Bool -> Just value] ->
      modifyIORef' optionsIORef $ set (#printer % #extraParens) value
    _ -> liftIO $ Text.hPutStrLn IO.stderr "Invalid option or argument(s)"

helpCommand :: IORef Options -> String -> HaskelineT IO ()
helpCommand optionsIORef arguments = do
  options <- readIORef optionsIORef

  let optionHelp :: (Is k A_Getter, Show a) => Optic' k is Options a -> Text
      optionHelp getter = Text.intercalate ", "
        [ "Default: " <> show (view getter defaultOptions)
        , "Current: " <> show (view getter options)
        ]

  case String.words arguments of
    [] -> do
      putTextLn $ unlines
        [ "Commands:"
        , "  <expr>                    Evaluate expression"
        , "  :lex <expr>               Lex into tokens"
        , "  :parse <expr>             Parse into expression"
        , "  :eval <expr>              Evaluate expression"
        , "  :help                     Print help text"
        , "  :set <option> <value>     Change options"
        , ""
        , "Options:"
        , "  printer.extraParens       Include more parens when printing"
        , "    " <> optionHelp (#printer % #extraParens)
        ]

    _ -> liftIO $ Text.hPutStrLn IO.stderr "Invalid argument(s)"

data Options = Options
  { printer :: Printer.Options
  }
  deriving stock (Generic, Show)

defaultOptions :: Options
defaultOptions = Options
  { printer = Printer.defaultOptions
  }
