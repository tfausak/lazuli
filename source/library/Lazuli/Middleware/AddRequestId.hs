module Lazuli.Middleware.AddRequestId where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
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
      header =
        ( Header.lazuliRequestId,
          Witch.into @ByteString.ByteString
            . Witch.into @(Witch.UTF_8 ByteString.ByteString)
            . Witch.into @Text.Text
            $ Witch.into @String requestId
        )
  handle request {Wai.vault = vault} $ \response -> do
    respond $ Wai.mapResponseHeaders (List.appendIfMissing header) response
