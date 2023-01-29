module Lazuli.Middleware.LogResponses where

import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as Text
import qualified Data.Vault.Lazy as Vault
import qualified GHC.Clock as Clock
import qualified Lazuli.Extra.Wai as Wai
import qualified Lazuli.Type.RequestId as RequestId
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.Printf as Printf
import qualified Witch

middleware :: Vault.Key RequestId.RequestId -> Wai.Middleware
middleware = middlewareWith Clock.getMonotonicTime putStrLn

middlewareWith ::
  (Monad m) =>
  m Double ->
  (String -> m ()) ->
  Vault.Key RequestId.RequestId ->
  Wai.MiddlewareWith m
middlewareWith getTime myPutStrLn key handle request respond = do
  before <- getTime
  handle request $ \response -> do
    after <- getTime
    myPutStrLn $
      Printf.printf
        "%s %s %s%s %d %.3f"
        (Witch.into @String . Maybe.fromMaybe (RequestId.RequestId 0) . Vault.lookup key $ Wai.vault request)
        (Text.decodeUtf8Lenient $ Wai.requestMethod request)
        (Text.decodeUtf8Lenient $ Wai.rawPathInfo request)
        (Text.decodeUtf8Lenient $ Wai.rawQueryString request)
        (Http.statusCode $ Wai.responseStatus response)
        (after - before)
    respond response
