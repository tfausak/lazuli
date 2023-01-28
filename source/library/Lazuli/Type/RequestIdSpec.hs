module Lazuli.Type.RequestIdSpec where

import qualified Lazuli.Type.RequestId as RequestId
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Type.RequestId" $ do
  Hspec.describe "Into String" $ do
    Hspec.it "pads with zeroes" $ do
      Witch.into @String (RequestId.RequestId 0) `Hspec.shouldBe` "00000000"

    Hspec.it "uses hexadecimal" $ do
      Witch.into @String (RequestId.RequestId 0x0123def0) `Hspec.shouldBe` "0123def0"
