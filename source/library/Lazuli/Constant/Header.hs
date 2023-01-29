module Lazuli.Constant.Header where

import qualified Network.HTTP.Types as Http

contentSecurityPolicy :: Http.HeaderName
contentSecurityPolicy = "Content-Security-Policy"

contentTypeOptions :: Http.HeaderName
contentTypeOptions = "X-Content-Type-Options"

frameOptions :: Http.HeaderName
frameOptions = "X-Frame-Options"

lazuliRequestId :: Http.HeaderName
lazuliRequestId = "Lazuli-Request-Id"

permissionsPolicy :: Http.HeaderName
permissionsPolicy = "Permissions-Policy"

referrerPolicy :: Http.HeaderName
referrerPolicy = "Referrer-Policy"

strictTransportSecurity :: Http.HeaderName
strictTransportSecurity = "Strict-Transport-Security"
