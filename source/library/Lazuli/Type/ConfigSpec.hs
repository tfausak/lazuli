module Lazuli.Type.ConfigSpec where

import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Flag as Flag
import qualified Lazuli.Type.Port as Port
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Type.Config" $ do
  Hspec.describe "applyFlag" $ do
    Hspec.it "handles the help flag" $ do
      Config.applyFlag Config.initial (Flag.Help True) `Hspec.shouldBe` Just Config.initial {Config.help = True}

    Hspec.it "handles the version flag" $ do
      Config.applyFlag Config.initial (Flag.Version True) `Hspec.shouldBe` Just Config.initial {Config.version = True}

    Hspec.it "sets a valid port" $ do
      Config.applyFlag Config.initial (Flag.Port "1234") `Hspec.shouldBe` Just Config.initial {Config.port = Port.Port 1234}

    Hspec.it "rejects an invalid port" $ do
      Config.applyFlag Config.initial (Flag.Port "port") `Hspec.shouldBe` Nothing
