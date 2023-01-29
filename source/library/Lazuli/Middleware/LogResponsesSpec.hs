module Lazuli.Middleware.LogResponsesSpec where

import qualified Control.Monad as Monad
import qualified Data.IORef as IORef
import qualified Data.Vault.Lazy as Vault
import qualified Lazuli.Middleware.LogResponses as LogResponses
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai.Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Middleware.LogResponses" $ do
  Hspec.describe "middleware" $ do
    Hspec.it "logs the response" $ do
      key <- Vault.newKey
      ref <- IORef.newIORef ""
      Monad.void
        . Wai.Test.withSession
          (LogResponses.middlewareWith (pure 0) (IORef.writeIORef ref) key application)
        . Wai.Test.request
        $ Wai.Test.setPath Wai.Test.defaultRequest "/p?q"
      IORef.readIORef ref `Hspec.shouldReturn` "00000000 GET /p?q 200 0.000"

application :: Wai.Application
application _ respond = respond $ Wai.responseLBS Http.status200 [] ""
