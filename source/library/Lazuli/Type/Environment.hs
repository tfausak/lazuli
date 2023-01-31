module Lazuli.Type.Environment where

import qualified Witch

data Environment
  = Development
  | Production
  | Testing
  deriving (Eq, Show)

instance Witch.TryFrom String Environment where
  tryFrom = Witch.maybeTryFrom $ \string -> case string of
    "development" -> Just Development
    "production" -> Just Production
    "testing" -> Just Testing
    _ -> Nothing

instance Witch.From Environment String where
  from environment = case environment of
    Development -> "development"
    Production -> "production"
    Testing -> "testing"
