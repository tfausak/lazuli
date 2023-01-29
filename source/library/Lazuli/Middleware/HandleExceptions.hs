module Lazuli.Middleware.HandleExceptions where

import qualified Control.Monad.Catch as Catch
import qualified Lazuli.Action.Exception.Handle as Exception.Handle
import qualified Lazuli.Extra.Wai as Wai
import qualified Lazuli.Server.Application as Application
import qualified Lazuli.Type.Context as Context
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Patrol
import qualified Patrol.Client
import qualified Patrol.Type.Event
import qualified System.IO as IO

middleware :: Context.Context -> Wai.Middleware
middleware = middlewareWith (IO.hPutStrLn IO.stderr) Patrol.Type.Event.new Patrol.Client.store

middlewareWith ::
  (Catch.MonadCatch m) =>
  (String -> m ()) ->
  m Patrol.Event ->
  (Client.Manager -> Patrol.Dsn -> Patrol.Event -> m Patrol.Response) ->
  Context.Context ->
  Wai.MiddlewareWith m
middlewareWith myPutStrLn newEvent store context handle request respond =
  Catch.catchAll (handle request respond) $ \exception -> do
    Exception.Handle.runWith myPutStrLn newEvent store context exception
    respond $ Application.statusResponse Http.internalServerError500
