module Lazuli.Server.Middleware where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vault.Lazy as Vault
import qualified GHC.Clock as Clock
import qualified Lazuli.Constant.Header as Header
import qualified Lazuli.Type.RequestId as RequestId
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified System.Random as Random
import qualified Text.Printf as Printf
import qualified Witch

middleware :: Vault.Key RequestId.RequestId -> Wai.Middleware
middleware requestIdKey handle request respond = do
  requestId <- Random.randomIO
  let vault = Vault.insert requestIdKey requestId $ Wai.vault request
  before <- Clock.getMonotonicTime
  handle request {Wai.vault = vault} $ \response -> do
    after <- Clock.getMonotonicTime
    Printf.printf
      "%s %s %s%s %d %.3f\n"
      (Witch.into @String requestId)
      (Text.decodeUtf8Lenient $ Wai.requestMethod request)
      (Text.decodeUtf8Lenient $ Wai.rawPathInfo request)
      (Text.decodeUtf8Lenient $ Wai.rawQueryString request)
      (Http.statusCode $ Wai.responseStatus response)
      (after - before)
    let header = (Header.lazuliRequestId, Text.encodeUtf8 . Text.pack $ Witch.into @String requestId)
    respond $ Wai.mapResponseHeaders (header :) response
