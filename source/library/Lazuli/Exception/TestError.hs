module Lazuli.Exception.TestError where

import qualified Control.Monad.Catch as Catch

data TestError
  = TestError
  deriving (Eq, Show)

instance Catch.Exception TestError
