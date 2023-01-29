module Lazuli.Middleware.HandleExceptions where

import qualified Control.Monad.Catch as Catch
import qualified Lazuli.Action.Exception.Handle as Exception.Handle
import qualified Lazuli.Extra.Wai as Wai
import qualified Lazuli.Server.Application as Application
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified System.IO as IO

middleware :: Wai.Middleware
middleware = middlewareWith $ IO.hPutStrLn IO.stderr

middlewareWith :: (Catch.MonadCatch m) => (String -> m ()) -> Wai.MiddlewareWith m
middlewareWith myPutStrLn handle request respond =
  Catch.catchAll (handle request respond) $ \exception -> do
    Exception.Handle.runWith myPutStrLn exception
    respond $ Application.statusResponse Http.internalServerError500
