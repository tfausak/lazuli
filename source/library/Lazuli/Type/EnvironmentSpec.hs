module Lazuli.Type.EnvironmentSpec where

import qualified Data.Either as Either
import qualified Lazuli.Extra.Either as Either
import qualified Lazuli.Type.Environment as Environment
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Type.Environment" $ do
  Hspec.describe "TryFrom String" $ do
    Hspec.it "works for development" $ do
      Either.hush (Witch.tryFrom @String "development") `Hspec.shouldBe` Just Environment.Development

    Hspec.it "works for production" $ do
      Either.hush (Witch.tryFrom @String "production") `Hspec.shouldBe` Just Environment.Production

    Hspec.it "works for testing" $ do
      Either.hush (Witch.tryFrom @String "testing") `Hspec.shouldBe` Just Environment.Testing

    Hspec.it "fails for other strings" $ do
      Witch.tryFrom @String @Environment.Environment "other" `Hspec.shouldSatisfy` Either.isLeft

  Hspec.describe "Into String" $ do
    Hspec.it "works for development" $ do
      Witch.into @String Environment.Development `Hspec.shouldBe` "development"

    Hspec.it "works for production" $ do
      Witch.into @String Environment.Production `Hspec.shouldBe` "production"

    Hspec.it "works for testing" $ do
      Witch.into @String Environment.Testing `Hspec.shouldBe` "testing"
