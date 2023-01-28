module Lazuli.Middleware.AddRequestId where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vault.Lazy as Vault
import qualified Lazuli.Constant.Header as Header
import qualified Lazuli.Extra.List as List
import qualified Lazuli.Extra.Wai as Wai
import qualified Lazuli.Type.RequestId as RequestId
import qualified Network.Wai as Wai
import qualified System.Random as Random
import qualified Witch

middleware :: Vault.Key RequestId.RequestId -> Wai.Middleware
middleware = middlewareWith Random.randomIO

middlewareWith ::
  (Monad m) =>
  m RequestId.RequestId ->
  Vault.Key RequestId.RequestId ->
  Wai.MiddlewareWith m
middlewareWith generate key handle request respond = do
  requestId <- generate
  let vault = Vault.insert key requestId $ Wai.vault request
      header = (Header.lazuliRequestId, Text.encodeUtf8 . Text.pack $ Witch.into @String requestId)
  handle request {Wai.vault = vault} $ \response -> do
    respond $ Wai.mapResponseHeaders (List.appendIfMissing header) response
