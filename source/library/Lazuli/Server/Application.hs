module Lazuli.Server.Application where

import qualified Lucid
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

application :: Wai.Application
application _request respond = respond
  . Wai.responseLBS
    Http.ok200
    [(Http.hContentType, "text/html;charset=utf-8")]
  . Lucid.renderBS
  $ do
    Lucid.doctype_
    Lucid.html_ [Lucid.lang_ "en-US"] $ do
      Lucid.head_ $ do
        Lucid.meta_ [Lucid.charset_ "utf-8"]
        Lucid.title_ "Lazuli"
      Lucid.body_ $ do
        Lucid.h1_ "Lazuli"
