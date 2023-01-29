module Lazuli.Type.UrlSpec where

import qualified Data.Either as Either
import qualified Lazuli.Type.Url as Url
import qualified Patrol
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Type.Url" $ do
  Hspec.describe "TryFrom String" $ do
    Hspec.it "succeds with a valid URL" $ do
      Witch.tryFrom @String @Url.Url "http://test" `Hspec.shouldSatisfy` Either.isRight

    Hspec.it "fails with an invalid URL" $ do
      Witch.tryFrom @String @Url.Url "invalid" `Hspec.shouldSatisfy` Either.isLeft

  Hspec.describe "TryInto Dsn" $ do
    Hspec.it "succeeds with a valid DSN" $ do
      Witch.tryVia @Url.Url @String @Patrol.Dsn "http://user@test" `Hspec.shouldSatisfy` Either.isRight

    Hspec.it "fails with an invalid DSN" $ do
      Witch.tryVia @Url.Url @String @Patrol.Dsn "http://test" `Hspec.shouldSatisfy` Either.isLeft
