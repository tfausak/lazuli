module Lazuli.Action.Config.LoadSpec where

import qualified Lazuli.Action.Config.Load as Config.Load
import qualified Lazuli.Type.Config as Config
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Action.Config.Load" $ do
  Hspec.describe "runWith" $ do
    Hspec.it "works with no flags" $ do
      Config.Load.runWith (pure . flip lookup []) [] `Hspec.shouldReturn` Config.initial

    Hspec.it "works with an environment variable" $ do
      Config.Load.runWith (pure . flip lookup [("LAZULI_HELP", "x")]) [] `Hspec.shouldReturn` Config.initial {Config.help = True}

    Hspec.it "works with a command line argument" $ do
      Config.Load.runWith (pure . flip lookup []) ["--help"] `Hspec.shouldReturn` Config.initial {Config.help = True}

    Hspec.it "prefers arguments over environment" $ do
      Config.Load.runWith (pure . flip lookup [("LAZULI_HELP", "x")]) ["--no-help"] `Hspec.shouldReturn` Config.initial
