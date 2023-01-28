module Lazuli.Server.Middleware where

import qualified Data.Text.Encoding as Text
import qualified Data.Vault.Lazy as Vault
import qualified GHC.Clock as Clock
import qualified Lazuli.Type.RequestId as RequestId
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Text.Printf as Printf
import qualified Witch

middleware :: Vault.Key RequestId.RequestId -> Wai.Middleware
middleware requestIdKey handle request respond = do
  before <- Clock.getMonotonicTime
  handle request $ \response -> do
    let maybeRequestId = Vault.lookup requestIdKey $ Wai.vault request
    after <- Clock.getMonotonicTime
    Printf.printf
      "%s %s %s%s %d %.3f\n"
      (maybe "unknown" (Witch.into @String) maybeRequestId)
      (Text.decodeUtf8Lenient $ Wai.requestMethod request)
      (Text.decodeUtf8Lenient $ Wai.rawPathInfo request)
      (Text.decodeUtf8Lenient $ Wai.rawQueryString request)
      (Http.statusCode $ Wai.responseStatus response)
      (after - before)
    respond response
