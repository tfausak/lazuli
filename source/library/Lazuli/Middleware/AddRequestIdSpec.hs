module Lazuli.Middleware.AddRequestIdSpec where

import qualified Data.Vault.Lazy as Vault
import qualified Lazuli.Constant.Header as Header
import qualified Lazuli.Middleware.AddRequestId as AddRequestId
import qualified Lazuli.Type.RequestId as RequestId
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai.Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Middleware.AddRequestId" $ do
  Hspec.describe "middleware" $ do
    Hspec.it "sets the request ID header" $ do
      key <- Vault.newKey
      response <-
        Wai.Test.withSession
          (AddRequestId.middlewareWith (pure $ RequestId.RequestId 0x0123def0) key application)
          $ Wai.Test.request Wai.Test.defaultRequest
      Wai.Test.simpleHeaders response `Hspec.shouldContain` [(Header.lazuliRequestId, "0123def0")]

application :: Wai.Application
application _ respond = respond $ Wai.responseLBS Http.status200 [] ""
