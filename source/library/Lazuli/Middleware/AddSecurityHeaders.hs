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
      [ (Header.contentSecurityPolicy, "default-src 'none'"),
        (Header.contentTypeOptions, "nosniff"),
        (Header.frameOptions, "DENY"),
        (Header.referrerPolicy, "no-referrer"),
        (Header.strictTransportSecurity, "max-age=31536000; includeSubDomains")
      ]
