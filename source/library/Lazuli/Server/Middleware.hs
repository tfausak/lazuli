module Lazuli.Server.Middleware where

import qualified Data.Vault.Lazy as Vault
import qualified Lazuli.Middleware.AddRequestId as AddRequestId
import qualified Lazuli.Middleware.AddSecurityHeaders as AddSecurityHeaders
import qualified Lazuli.Middleware.HandleExceptions as HandleExceptions
import qualified Lazuli.Middleware.LogResponses as LogResponses
import qualified Lazuli.Type.RequestId as RequestId
import qualified Network.Wai as Wai

middleware :: Vault.Key RequestId.RequestId -> Wai.Middleware
middleware key =
  AddRequestId.middleware key
    . LogResponses.middleware key
    . AddSecurityHeaders.middleware
    . HandleExceptions.middleware
