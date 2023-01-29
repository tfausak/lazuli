module Lazuli.Server.Middleware where

import qualified Lazuli.Middleware.AddRequestId as AddRequestId
import qualified Lazuli.Middleware.AddSecurityHeaders as AddSecurityHeaders
import qualified Lazuli.Middleware.HandleExceptions as HandleExceptions
import qualified Lazuli.Middleware.LogResponses as LogResponses
import qualified Lazuli.Type.Context as Context
import qualified Network.Wai as Wai

middleware :: Context.Context -> Wai.Middleware
middleware context =
  AddRequestId.middleware (Context.requestIdKey context)
    . LogResponses.middleware (Context.requestIdKey context)
    . AddSecurityHeaders.middleware
    . HandleExceptions.middleware context
