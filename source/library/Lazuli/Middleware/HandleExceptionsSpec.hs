module Lazuli.Middleware.HandleExceptionsSpec where

import qualified Control.Monad.Catch as Catch
import qualified Lazuli.Action.Context.Load as Context.Load
import qualified Lazuli.Exception.TestError as TestError
import qualified Lazuli.Middleware.HandleExceptions as HandleExceptions
import qualified Lazuli.Type.Config as Config
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai.Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Middleware.HandleExceptions" $ do
  Hspec.describe "middleware" $ do
    Hspec.it "returns a 500" $ do
      context <- Context.Load.run Config.initial
      response <-
        Wai.Test.runSession (Wai.Test.request Wai.Test.defaultRequest) $
          HandleExceptions.middlewareWith
            (const $ pure ())
            (error "unused")
            (error "unused")
            context
            application
      Wai.Test.simpleStatus response `Hspec.shouldBe` Http.internalServerError500

application :: Wai.Application
application _ _ = Catch.throwM TestError.TestError
