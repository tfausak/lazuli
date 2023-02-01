module Lazuli.Server.Application where

import qualified Control.Monad.Catch as Catch
import qualified Data.Text as Text
import qualified Lazuli.Constant.Mime as Mime
import qualified Lazuli.Constant.Version as Version
import qualified Lazuli.Exception.TestError as TestError
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Context as Context
import qualified Lucid
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified System.FilePath as FilePath
import qualified Witch

application :: Context.Context -> Wai.Application
application context request respond =
  case (Wai.requestMethod request, Wai.pathInfo request) of
    ("GET", []) -> respond
      . Wai.responseLBS
        Http.ok200
        [(Http.hContentType, Mime.textHtml)]
      . Lucid.renderBS
      $ do
        Lucid.doctype_
        Lucid.html_ [Lucid.lang_ "en-US"] $ do
          Lucid.head_ $ do
            Lucid.meta_ [Lucid.charset_ "utf-8"]
            Lucid.meta_ [Lucid.name_ "viewport", Lucid.content_ "initial-scale=1, width=device-width"]
            Lucid.title_ "Lazuli"
            Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ $ Config.baseUrl (Context.config context) <> "static/bootstrap.css"]
            Lucid.link_ [Lucid.rel_ "canonical", Lucid.href_ . Config.baseUrl $ Context.config context]
          Lucid.body_ $ do
            Lucid.header_ $ do
              Lucid.nav_ [Lucid.class_ "bg-body-secondary navbar"] $ do
                Lucid.div_ [Lucid.class_ "container"] $ do
                  Lucid.a_ [Lucid.class_ "navbar-brand", Lucid.href_ . Config.baseUrl $ Context.config context] "Lazuli"
            Lucid.main_ [Lucid.class_ "my-3"] $ do
              Lucid.div_ [Lucid.class_ "container"] $ do
                Lucid.p_ $ do
                  "\x1f48e"
            Lucid.footer_ [Lucid.class_ "text-secondary"] $ do
              Lucid.div_ [Lucid.class_ "border-top container py-3"] $ do
                Lucid.p_ $ do
                  "Powered by "
                  Lucid.a_ [Lucid.href_ "https://github.com/tfausak/lazuli"] "Lazuli"
                  " version "
                  Lucid.toHtml Version.string
                  case Config.commit $ Context.config context of
                    Nothing -> pure ()
                    Just commit -> do
                      " commit "
                      Lucid.a_ [Lucid.href_ $ "https://github.com/tfausak/lazuli/commit/" <> commit] $ do
                        Lucid.toHtml $ Text.take 7 commit
                  "."
    ("GET", ["api", "health-check"]) -> respond $ statusResponse Http.ok200 [(Http.hCacheControl, "no-cache")]
    ("POST", ["api", "throw"]) -> Catch.throwM TestError.TestError
    ("GET", ["favicon.ico"]) ->
      respond $
        Wai.responseFile
          Http.ok200
          [ (Http.hCacheControl, "max-age=86400, stale-while-revalidate=3600"),
            (Http.hContentType, Mime.imageIcon)
          ]
          (FilePath.combine (Config.dataDirectory $ Context.config context) "favicon.ico")
          Nothing
    ("GET", ["robots.txt"]) ->
      respond $
        Wai.responseLBS
          Http.ok200
          [ (Http.hCacheControl, "max-age=86400, stale-while-revalidate=3600"),
            (Http.hContentType, Mime.textPlain)
          ]
          "User-agent: *\nAllow: /\n"
    ("GET", ["static", "bootstrap.css"]) ->
      respond $
        Wai.responseFile
          Http.ok200
          [ (Http.hCacheControl, "max-age=86400, stale-while-revalidate=3600"),
            (Http.hContentType, Mime.textCss)
          ]
          (FilePath.combine (Config.dataDirectory $ Context.config context) "bootstrap.css")
          Nothing
    _ -> respond $ statusResponse Http.notFound404 []

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
  Wai.responseLBS status ((Http.hContentType, Mime.textPlain) : headers)
    . Witch.from
    . (<> "\n")
    $ Http.statusMessage status
