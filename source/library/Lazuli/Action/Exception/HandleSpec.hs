module Lazuli.Action.Exception.HandleSpec where

import qualified Control.Monad.Catch as Catch
import qualified Data.IORef as IORef
import qualified Lazuli.Action.Context.Load as Context.Load
import qualified Lazuli.Action.Exception.Handle as Exception.Handle
import qualified Lazuli.Exception.TestError as TestError
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Url as Url
import qualified Patrol.Type.Event
import qualified Patrol.Type.EventId
import qualified Patrol.Type.Response
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Action.Exception.Handle" $ do
  Hspec.describe "runWith" $ do
    Hspec.it "logs the exception" $ do
      ref <- IORef.newIORef ""
      context <- Context.Load.run Config.initial
      Exception.Handle.runWith (IORef.writeIORef ref) (error "unused") (error "unused") context TestError.TestError
      IORef.readIORef ref `Hspec.shouldReturn` "TestError"

    Hspec.it "sends the exception to Sentry" $ do
      ref <- IORef.newIORef False
      dsn <- either Catch.throwM pure $ Witch.tryVia @Url.Url @String "http://user@test"
      context <- Context.Load.run Config.initial {Config.sentryDsn = Just dsn}
      let response = Patrol.Type.Response.Response {Patrol.Type.Response.id = Patrol.Type.EventId.empty}
      Exception.Handle.runWith
        (const $ pure ())
        (pure Patrol.Type.Event.empty)
        (\_ _ _ -> IORef.atomicModifyIORef ref $ const (True, response))
        context
        TestError.TestError
      IORef.readIORef ref `Hspec.shouldReturn` True
