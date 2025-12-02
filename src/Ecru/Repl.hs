module Ecru.Repl
  ( repl
  , replWithOptions
  , Options (..)
  , defaultOptions
  )
where

import Data.String qualified as String
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Ecru.Eval (eval, prelude)
import Ecru.Lex (lex)
import Ecru.Parse (parse)
import Ecru.Print (Print (..))
import Ecru.Print qualified as Print
import Optics
import Prelude hiding (print)
import System.Console.Repline hiding (Options)
import System.IO qualified as IO
import Text.Pretty.Simple (pPrint)

repl :: MonadIO m => m ()
repl = replWithOptions defaultOptions

replWithOptions :: MonadIO m => Options -> m ()
replWithOptions options = liftIO do
  optionsIORef <- newIORef options

  evalReplOpts ReplOpts
    { banner = \case
        SingleLine -> pure "ecru> "
        MultiLine -> pure "ecru| "
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

type Repl = HaskelineT IO

lexCommand :: IORef Options -> String -> Repl ()
lexCommand optionsIORef source = do
  Options{printer} <- readIORef optionsIORef

  case lex (toText source) of
    Left err -> liftIO $ Text.hPutStrLn IO.stderr err
    Right value -> do
      putTextLn $ printWithOptions printer value
      pPrint value

parseCommand :: IORef Options -> String -> Repl ()
parseCommand optionsIORef source = do
  Options{printer} <- readIORef optionsIORef

  case lex (toText source) >>= parse of
    Left err -> liftIO $ Text.hPutStrLn IO.stderr err
    Right value -> do
      putTextLn $ printWithOptions printer value
      pPrint value

evalCommand :: IORef Options -> String -> Repl ()
evalCommand optionsIORef source = do
  Options{printer} <- readIORef optionsIORef

  case lex (toText source) >>= parse <&> eval prelude of
    Left err -> liftIO $ Text.hPutStrLn IO.stderr err
    Right value -> putTextLn $ printWithOptions printer value

setCommand :: IORef Options -> String -> Repl ()
setCommand optionsIORef arguments = do
  case String.words arguments of
    ["printer.extraParens", readMaybe @Bool -> Just value] ->
      modifyIORef' optionsIORef $ set (#printer % #extraParens) value
    _ -> liftIO $ Text.hPutStrLn IO.stderr "Invalid option or argument(s)"

helpCommand :: IORef Options -> String -> Repl ()
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
  { printer :: Print.Options
  }
  deriving stock (Generic)

defaultOptions :: Options
defaultOptions = Options
  { printer = Print.defaultOptions
  }
