module Lazuli.Server.Application where

import qualified Control.Monad.Catch as Catch
import qualified Lazuli.Exception.TestError as TestError
import qualified Lucid
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Witch

application :: Wai.Application
application request respond =
  case (Wai.requestMethod request, Wai.pathInfo request) of
    ("GET", []) -> respond
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
    ("GET", ["api", "health-check"]) -> respond $ statusResponse Http.ok200
    ("POST", ["api", "throw"]) -> Catch.throwM TestError.TestError
    _ -> respond $ statusResponse Http.notFound404

statusResponse :: Http.Status -> Wai.Response
statusResponse status =
  Wai.responseLBS status [(Http.hContentType, "text/plain;charset=utf-8")]
    . Witch.from
    $ Http.statusMessage status
