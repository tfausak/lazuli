module Lazuli.Log where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Time as Time
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Context as Context
import qualified Lazuli.Type.LogLevel as LogLevel
import qualified System.IO as IO
import qualified Witch

debug :: Context.Context -> String -> IO ()
debug = hLog IO.stdout LogLevel.Debug

info :: Context.Context -> String -> IO ()
info = hLog IO.stdout LogLevel.Info

warn :: Context.Context -> String -> IO ()
warn = hLog IO.stderr LogLevel.Warn

error :: Context.Context -> String -> IO ()
error = hLog IO.stderr LogLevel.Error

hLog :: IO.Handle -> LogLevel.LogLevel -> Context.Context -> String -> IO ()
hLog handle logLevel context message = Monad.when (logLevel >= Config.logLevel (Context.config context)) $ do
  now <- Time.getCurrentTime
  ByteString.hPut handle
    . Witch.via @(Witch.UTF_8 ByteString.ByteString)
    $ mconcat
      [ Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" now,
        " [",
        Witch.into @String logLevel,
        "] ",
        message,
        "\n"
      ]
