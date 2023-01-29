module Lazuli.Action.Context.Load where

import qualified Data.Vault.Lazy as Vault
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Context as Context
import qualified Network.HTTP.Client.TLS as Tls

run :: Config.Config -> IO Context.Context
run config = do
  manager <- Tls.newTlsManager
  requestIdKey <- Vault.newKey
  pure
    Context.Context
      { Context.config = config,
        Context.manager = manager,
        Context.requestIdKey = requestIdKey
      }
