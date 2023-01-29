module Lazuli.Type.Context where

import qualified Data.Vault.Lazy as Vault
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.RequestId as RequestId
import qualified Network.HTTP.Client as Client

data Context = Context
  { config :: Config.Config,
    manager :: Client.Manager,
    requestIdKey :: Vault.Key RequestId.RequestId
  }
