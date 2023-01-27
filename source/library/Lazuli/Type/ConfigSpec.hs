module Lazuli.Type.ConfigSpec where

import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Flag as Flag
import qualified Lazuli.Type.Port as Port
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Type.Config" $ do
  Hspec.describe "applyFlag" $ do
    Hspec.it "handles the help flag" $ do
      Config.applyFlag Config.initial Flag.Help `Hspec.shouldBe` Just Config.initial {Config.help = True}

    Hspec.it "handles the version flag" $ do
      Config.applyFlag Config.initial Flag.Version `Hspec.shouldBe` Just Config.initial {Config.version = True}

    Hspec.it "sets a valid port" $ do
      Config.applyFlag Config.initial (Flag.Port "1234") `Hspec.shouldBe` Just Config.initial {Config.port = Port.Port 1234}

    Hspec.it "rejects an invalid port" $ do
      Config.applyFlag Config.initial (Flag.Port "port") `Hspec.shouldBe` Nothing

  Hspec.describe "fromArguments" $ do
    Hspec.it "with no arguments, returns the initial config" $ do
      Config.fromArguments [] `Hspec.shouldBe` Just Config.initial

    Hspec.it "rejects unexpected arguments" $ do
      Config.fromArguments ["unexpected"] `Hspec.shouldBe` Nothing

    Hspec.it "rejects unknown options" $ do
      Config.fromArguments ["--unknown"] `Hspec.shouldBe` Nothing

    Hspec.it "rejects invalid options" $ do
      Config.fromArguments ["--help=invalid"] `Hspec.shouldBe` Nothing

    Hspec.it "sets one flag" $ do
      Config.fromArguments ["--help"] `Hspec.shouldBe` Just Config.initial {Config.help = True}

    Hspec.it "sets multiple flags" $ do
      Config.fromArguments ["--help", "--version"] `Hspec.shouldBe` Just Config.initial {Config.help = True, Config.version = True}
