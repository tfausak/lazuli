module Lazuli.Action.Context.Load where

import qualified Data.Vault.Lazy as Vault
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Context as Context

run :: Config.Config -> IO Context.Context
run config = do
  requestIdKey <- Vault.newKey
  pure
    Context.Context
      { Context.config = config,
        Context.requestIdKey = requestIdKey
      }
