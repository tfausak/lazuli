module Lazuli.Type.Port where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read
import qualified Witch

newtype Port
  = Port Warp.Port
  deriving (Eq, Show)

instance Witch.TryFrom String Port where
  tryFrom = Witch.maybeTryFrom $ \s -> case Read.readMaybe s of
    Nothing -> Nothing
    Just p -> Just $ Port p

instance Witch.From Port Warp.Port
