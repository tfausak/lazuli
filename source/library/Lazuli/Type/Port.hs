module Lazuli.Type.Port where

import qualified Lazuli.Exception.InvalidPort as InvalidPort
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Read as Read

newtype Port
  = Port Warp.Port
  deriving (Eq, Show)

fromString :: String -> Either InvalidPort.InvalidPort Port
fromString s = case Read.readMaybe s of
  Nothing -> Left $ InvalidPort.InvalidPort s
  Just p -> Right $ Port p

intoPort :: Port -> Warp.Port
intoPort (Port p) = p
