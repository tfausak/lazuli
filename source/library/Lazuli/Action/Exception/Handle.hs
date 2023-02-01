module Lazuli.Action.Exception.Handle where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as Text
import qualified Lazuli.Log as Log
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Context as Context
import qualified Network.HTTP.Client as Client
import qualified Network.Wai as Wai
import qualified Patrol
import qualified Patrol.Client
import qualified Patrol.Type.Event
import qualified Patrol.Type.Exception
import qualified Patrol.Type.Exceptions
import qualified Patrol.Type.Request
import qualified Patrol.Type.Response
import qualified System.Environment as Environment
import qualified Witch

run :: (Catch.Exception e) => Context.Context -> Maybe Wai.Request -> e -> IO ()
run context = runWith (Log.error context) Patrol.Type.Event.new Environment.getEnvironment Patrol.Client.store context

runWith ::
  (Monad m, Catch.Exception e) =>
  (String -> m ()) ->
  m Patrol.Event ->
  m [(String, String)] ->
  (Client.Manager -> Patrol.Dsn -> Patrol.Event -> m Patrol.Response) ->
  Context.Context ->
  Maybe Wai.Request ->
  e ->
  m ()
runWith myPutStrLn newEvent getEnvironment store context maybeRequest exception = do
  myPutStrLn $ Catch.displayException exception
  case Config.sentryDsn $ Context.config context of
    Nothing -> pure ()
    Just sentryDsn -> do
      event <- newEvent
      environment <- getEnvironment
      let patrolException = Patrol.Type.Exception.fromSomeException $ Catch.toException exception
          patrolExceptions = Patrol.Type.Exceptions.empty {Patrol.Type.Exceptions.values = [patrolException]}
          patrolRequest =
            Patrol.Type.Request.empty
              { Patrol.Type.Request.env = Map.fromList $ fmap (Bifunctor.bimap Witch.from Aeson.toJSON) environment,
                Patrol.Type.Request.headers = maybe Map.empty (Map.fromList . fmap (Bifunctor.bimap (Text.decodeUtf8Lenient . CI.foldedCase) Text.decodeUtf8Lenient) . Wai.requestHeaders) maybeRequest,
                Patrol.Type.Request.method = maybe "" (Text.decodeUtf8Lenient . Wai.requestMethod) maybeRequest,
                Patrol.Type.Request.queryString = maybe Map.empty (Map.fromList . fmap (Bifunctor.bimap Text.decodeUtf8Lenient $ Text.decodeUtf8Lenient . Maybe.fromMaybe ByteString.empty) . Wai.queryString) maybeRequest,
                Patrol.Type.Request.url = maybe "" (Text.decodeUtf8Lenient . Wai.rawPathInfo) maybeRequest
              }
      response <-
        store
          (Context.manager context)
          sentryDsn
          event
            { Patrol.Type.Event.exception = Just patrolExceptions,
              Patrol.Type.Event.release = Maybe.fromMaybe "" . Config.commit $ Context.config context,
              Patrol.Type.Event.request = Just patrolRequest
            }
      myPutStrLn . show $ Patrol.Type.Response.id response
