module Lazuli.Action.Exception.Handle where

import qualified Control.Monad.Catch as Catch
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Context as Context
import qualified Network.HTTP.Client as Client
import qualified Patrol
import qualified Patrol.Client
import qualified Patrol.Type.Event
import qualified Patrol.Type.Exception
import qualified Patrol.Type.Exceptions
import qualified Patrol.Type.Response
import qualified System.IO as IO

run :: (Catch.Exception e) => Context.Context -> e -> IO ()
run = runWith (IO.hPutStrLn IO.stderr) Patrol.Type.Event.new Patrol.Client.store

runWith ::
  (Monad m, Catch.Exception e) =>
  (String -> m ()) ->
  m Patrol.Event ->
  (Client.Manager -> Patrol.Dsn -> Patrol.Event -> m Patrol.Response) ->
  Context.Context ->
  e ->
  m ()
runWith myPutStrLn newEvent store context exception = do
  myPutStrLn $ Catch.displayException exception
  case Config.sentryDsn $ Context.config context of
    Nothing -> pure ()
    Just sentryDsn -> do
      event <- newEvent
      let patrolException = Patrol.Type.Exception.fromSomeException $ Catch.toException exception
          patrolExceptions = Patrol.Type.Exceptions.empty {Patrol.Type.Exceptions.values = [patrolException]}
      response <-
        store
          (Context.manager context)
          sentryDsn
          event {Patrol.Type.Event.exception = Just patrolExceptions}
      myPutStrLn . show $ Patrol.Type.Response.id response
