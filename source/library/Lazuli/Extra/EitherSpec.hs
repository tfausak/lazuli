module Lazuli.Extra.EitherSpec where

import qualified Lazuli.Exception.TestError as TestError
import qualified Lazuli.Extra.Either as Either
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Extra.Either" $ do
  Hspec.describe "hush" $ do
    Hspec.it "converts Left to Nothing" $ do
      Either.hush (Left 0 :: Either Int ()) `Hspec.shouldBe` Nothing

    Hspec.it "converts Right to Just" $ do
      Either.hush (Right 0 :: Either () Int) `Hspec.shouldBe` Just 0

  Hspec.describe "throw" $ do
    Hspec.it "throws Left" $ do
      Either.throw (Left TestError.TestError :: Either TestError.TestError ()) `Hspec.shouldBe` Nothing

    Hspec.it "returns Right" $ do
      Either.hush (Right 0 :: Either TestError.TestError Int) `Hspec.shouldBe` Just 0
