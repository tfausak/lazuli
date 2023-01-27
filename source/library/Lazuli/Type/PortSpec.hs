module Lazuli.Type.PortSpec where

import qualified Data.Either as Either
import qualified Lazuli.Type.Port as Port
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Type.Port" $ do
  Hspec.describe "fromString" $ do
    Hspec.it "succeeds with a valid port" $ do
      Port.fromString "1234" `Hspec.shouldSatisfy` Either.isRight

    Hspec.it "fails with an invalid port" $ do
      Port.fromString "invalid" `Hspec.shouldSatisfy` Either.isLeft
