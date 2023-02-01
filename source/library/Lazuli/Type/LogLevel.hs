module Lazuli.Type.LogLevel where

import qualified Witch

data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving (Eq, Ord, Show)

instance Witch.TryFrom String LogLevel where
  tryFrom = Witch.maybeTryFrom $ \string -> case string of
    "debug" -> Just Debug
    "info" -> Just Info
    "warn" -> Just Warn
    "error" -> Just Error
    _ -> Nothing

instance Witch.From LogLevel String where
  from logLevel = case logLevel of
    Debug -> "debug"
    Info -> "info"
    Warn -> "warn"
    Error -> "error"
