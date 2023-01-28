module Lazuli.Extra.Hspec where

import qualified Test.Hspec as Hspec

exceptionType :: Hspec.Selector a
exceptionType = const True
