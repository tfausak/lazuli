module Lazuli.Action.Context.LoadSpec where

import qualified Lazuli.Action.Context.Load as Context.Load
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Context as Context
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Action.Context.Load" $ do
  Hspec.describe "runWith" $ do
    Hspec.it "sets the config" $ do
      let config = Config.testing
      context <- Context.Load.run config
      Context.config context `Hspec.shouldBe` config
