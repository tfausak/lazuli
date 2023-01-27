module Lazuli.Exception.UnknownOption where

import qualified Control.Exception as Exception

newtype UnknownOption
  = UnknownOption String
  deriving (Eq, Show)

instance Exception.Exception UnknownOption
