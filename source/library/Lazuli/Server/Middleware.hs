module Lazuli.Server.Middleware where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vault.Lazy as Vault
import qualified Data.Word as Word
import qualified Lazuli.Constant.Header as Header
import qualified Lazuli.Type.RequestId as RequestId
import qualified Network.Wai as Wai
import qualified System.Random as Random
import qualified Text.Printf as Printf
import qualified Witch

middleware :: Vault.Key RequestId.RequestId -> Wai.Middleware
middleware requestIdKey handle request respond = do
  requestId <- Random.randomIO
  let vault = Vault.insert requestIdKey requestId $ Wai.vault request
  handle request {Wai.vault = vault} $ \response -> do
    respond $ Wai.mapResponseHeaders ((Header.lazuliRequestId, Text.encodeUtf8 . Text.pack . Printf.printf "%016x" $ Witch.into @Word.Word64 requestId) :) response
