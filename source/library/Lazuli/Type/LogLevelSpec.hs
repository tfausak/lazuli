module Lazuli.Type.LogLevelSpec where

import qualified Data.Either as Either
import qualified Data.List as List
import qualified Lazuli.Extra.Either as Either
import qualified Lazuli.Type.LogLevel as LogLevel
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Type.LogLevel" $ do
  Hspec.describe "Ord" $ do
    Hspec.it "can be sorted" $ do
      List.sort [LogLevel.Error, LogLevel.Warn, LogLevel.Info, LogLevel.Debug] `Hspec.shouldBe` [LogLevel.Debug, LogLevel.Info, LogLevel.Warn, LogLevel.Error]

  Hspec.describe "TryFrom String" $ do
    Hspec.it "succeeds for debug" $ do
      Either.hush (Witch.tryFrom @String "debug") `Hspec.shouldBe` Just LogLevel.Debug

    Hspec.it "succeeds for info" $ do
      Either.hush (Witch.tryFrom @String "info") `Hspec.shouldBe` Just LogLevel.Info

    Hspec.it "succeeds for warn" $ do
      Either.hush (Witch.tryFrom @String "warn") `Hspec.shouldBe` Just LogLevel.Warn

    Hspec.it "succeeds for error" $ do
      Either.hush (Witch.tryFrom @String "error") `Hspec.shouldBe` Just LogLevel.Error

    Hspec.it "fails for an invalid level" $ do
      Witch.tryFrom @String @LogLevel.LogLevel "" `Hspec.shouldSatisfy` Either.isLeft

  Hspec.describe "Into String" $ do
    Hspec.it "works for debug" $ do
      Witch.into @String LogLevel.Debug `Hspec.shouldBe` "debug"

    Hspec.it "works for info" $ do
      Witch.into @String LogLevel.Info `Hspec.shouldBe` "info"

    Hspec.it "works for warn" $ do
      Witch.into @String LogLevel.Warn `Hspec.shouldBe` "warn"

    Hspec.it "works for error" $ do
      Witch.into @String LogLevel.Error `Hspec.shouldBe` "error"
