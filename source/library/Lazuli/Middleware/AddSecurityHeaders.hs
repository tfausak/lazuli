module Lazuli.Middleware.AddSecurityHeaders where

import qualified Lazuli.Constant.Header as Header
import qualified Lazuli.Extra.List as List
import qualified Network.Wai as Wai

middleware :: Wai.Middleware
middleware =
  Wai.modifyResponse . Wai.mapResponseHeaders $ \headers ->
    foldr
      List.appendIfMissing
      headers
      [ (Header.contentSecurityPolicy, "base-uri 'none'; default-src 'none'; form-action 'none'; frame-ancestors 'none'"),
        (Header.contentTypeOptions, "nosniff"),
        (Header.frameOptions, "DENY"),
        (Header.permissionsPolicy, "camera=(), microphone=()"),
        (Header.referrerPolicy, "no-referrer"),
        (Header.strictTransportSecurity, "max-age=31536000; includeSubDomains"),
        (Header.xssProtection, "1; mode=block")
      ]
