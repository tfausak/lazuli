module Lazuli.Type.ConfigSpec where

import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Flag as Flag
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Type.Config" $ do
  Hspec.describe "applyFlag" $ do
    Hspec.it "handles the help flag" $ do
      Config.applyFlag Config.initial Flag.Help `Hspec.shouldBe` Just Config.initial {Config.help = True}

    Hspec.it "handles the version flag" $ do
      Config.applyFlag Config.initial Flag.Version `Hspec.shouldBe` Just Config.initial {Config.version = True}
