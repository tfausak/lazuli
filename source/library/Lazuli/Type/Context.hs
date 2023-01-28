module Lazuli.Type.Context where

import qualified Data.Vault.Lazy as Vault
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.RequestId as RequestId

data Context = Context
  { config :: Config.Config,
    requestIdKey :: Vault.Key RequestId.RequestId
  }
