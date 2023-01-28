module Lazuli.Extra.HspecSpec where

import qualified Lazuli.Extra.Hspec as Hspec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Extra.Hspec" $ do
  Hspec.describe "exceptionType" $ do
    Hspec.it "works" $ do
      () `Hspec.shouldSatisfy` Hspec.exceptionType
