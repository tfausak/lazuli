module Lazuli.Server.Application where

import qualified Lazuli.Constant.Header as Header
import qualified Lucid
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

application :: Wai.Application
application _request respond = respond
  . Wai.responseLBS
    Http.ok200
    [ (Header.contentSecurityPolicy, "default-src 'none'"),
      (Http.hContentType, "text/html;charset=utf-8"),
      (Header.contentTypeOptions, "nosniff"),
      (Header.frameOptions, "DENY"),
      (Header.referrerPolicy, "no-referrer"),
      (Header.strictTransportSecurity, "max-age=31536000; includeSubDomains")
    ]
  . Lucid.renderBS
  $ do
    Lucid.doctype_
    Lucid.html_ [Lucid.lang_ "en-US"] $ do
      Lucid.head_ $ do
        Lucid.meta_ [Lucid.charset_ "utf-8"]
        Lucid.title_ "Lazuli"
      Lucid.body_ $ do
        Lucid.h1_ "Lazuli"
