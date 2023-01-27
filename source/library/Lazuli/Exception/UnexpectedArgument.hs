module Lazuli.Exception.UnexpectedArgument where

import qualified Control.Exception as Exception

newtype UnexpectedArgument
  = UnexpectedArgument String
  deriving (Eq, Show)

instance Exception.Exception UnexpectedArgument
