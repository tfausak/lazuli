module Lazuli.Exception.InvalidOption where

import qualified Control.Exception as Exception

newtype InvalidOption
  = InvalidOption String
  deriving (Eq, Show)

instance Exception.Exception InvalidOption
