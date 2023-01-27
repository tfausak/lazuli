module Lazuli.Exception.InvalidPort where

import qualified Control.Monad.Catch as Catch

newtype InvalidPort
  = InvalidPort String
  deriving (Eq, Show)

instance Catch.Exception InvalidPort
