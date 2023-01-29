module Lazuli.Middleware.HandleExceptionsSpec where

import qualified Control.Monad.Catch as Catch
import qualified Data.IORef as IORef
import qualified Lazuli.Exception.TestError as TestError
import qualified Lazuli.Middleware.HandleExceptions as HandleExceptions
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai.Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Middleware.HandleExceptions" $ do
  Hspec.describe "middleware" $ do
    Hspec.it "works" $ do
      ref <- IORef.newIORef ""
      response <-
        Wai.Test.withSession
          (HandleExceptions.middlewareWith (IORef.writeIORef ref) application)
          $ Wai.Test.request Wai.Test.defaultRequest
      Wai.Test.simpleStatus response `Hspec.shouldBe` Http.internalServerError500
      IORef.readIORef ref `Hspec.shouldReturn` "TestError"

application :: Wai.Application
application _ _ = Catch.throwM TestError.TestError
