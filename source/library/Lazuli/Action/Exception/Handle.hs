module Lazuli.Action.Exception.Handle where

import qualified Control.Monad.Catch as Catch
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Context as Context
import qualified Network.HTTP.Client as Client
import qualified Patrol
import qualified Patrol.Client
import qualified Patrol.Type.Event
import qualified Patrol.Type.Exception
import qualified Patrol.Type.Exceptions
import qualified Patrol.Type.Request
import qualified Patrol.Type.Response
import qualified System.Environment as Environment
import qualified System.IO as IO
import qualified Witch

run :: (Catch.Exception e) => Context.Context -> e -> IO ()
run = runWith (IO.hPutStrLn IO.stderr) Patrol.Type.Event.new Environment.getEnvironment Patrol.Client.store

runWith ::
  (Monad m, Catch.Exception e) =>
  (String -> m ()) ->
  m Patrol.Event ->
  m [(String, String)] ->
  (Client.Manager -> Patrol.Dsn -> Patrol.Event -> m Patrol.Response) ->
  Context.Context ->
  e ->
  m ()
runWith myPutStrLn newEvent getEnvironment store context exception = do
  myPutStrLn $ Catch.displayException exception
  case Config.sentryDsn $ Context.config context of
    Nothing -> pure ()
    Just sentryDsn -> do
      event <- newEvent
      environment <- getEnvironment
      let patrolException = Patrol.Type.Exception.fromSomeException $ Catch.toException exception
          patrolExceptions = Patrol.Type.Exceptions.empty {Patrol.Type.Exceptions.values = [patrolException]}
          patrolRequest = Patrol.Type.Request.empty {Patrol.Type.Request.env = Map.fromList $ fmap (Bifunctor.bimap Witch.from Aeson.toJSON) environment}
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
