module Lazuli.Type.PortSpec where

import qualified Data.Either as Either
import qualified Lazuli.Type.Port as Port
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Type.Port" $ do
  Hspec.describe "fromString" $ do
    Hspec.it "succeeds with a valid port" $ do
      Witch.tryFrom @String @Port.Port "1234" `Hspec.shouldSatisfy` Either.isRight

    Hspec.it "fails with an invalid port" $ do
      Witch.tryFrom @String @Port.Port "invalid" `Hspec.shouldSatisfy` Either.isLeft
