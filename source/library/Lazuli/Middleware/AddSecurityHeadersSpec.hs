module Lazuli.Middleware.AddSecurityHeadersSpec where

import qualified Lazuli.Constant.Header as Header
import qualified Lazuli.Middleware.AddSecurityHeaders as AddSecurityHeaders
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai.Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Middleware.AddSecurityHeaders" $ do
  Hspec.describe "middleware" $ do
    Hspec.it "sets the content security policy header" $ do
      headers <- getHeaders emptyApplication
      fmap fst headers `Hspec.shouldContain` [Header.contentSecurityPolicy]

    Hspec.it "sets the content type options header" $ do
      headers <- getHeaders emptyApplication
      fmap fst headers `Hspec.shouldContain` [Header.contentTypeOptions]

    Hspec.it "sets the feature policy header" $ do
      headers <- getHeaders emptyApplication
      fmap fst headers `Hspec.shouldContain` [Header.featurePolicy]

    Hspec.it "sets the frame options header" $ do
      headers <- getHeaders emptyApplication
      fmap fst headers `Hspec.shouldContain` [Header.frameOptions]

    Hspec.it "sets the permissions policy header" $ do
      headers <- getHeaders emptyApplication
      fmap fst headers `Hspec.shouldContain` [Header.permissionsPolicy]

    Hspec.it "sets the referrer policy header" $ do
      headers <- getHeaders emptyApplication
      fmap fst headers `Hspec.shouldContain` [Header.referrerPolicy]

    Hspec.it "sets the strict transport security header" $ do
      headers <- getHeaders emptyApplication
      fmap fst headers `Hspec.shouldContain` [Header.strictTransportSecurity]

    Hspec.it "sets the strict transport security header" $ do
      headers <- getHeaders emptyApplication
      fmap fst headers `Hspec.shouldContain` [Header.strictTransportSecurity]

    Hspec.it "sets the xss protection header" $ do
      headers <- getHeaders emptyApplication
      fmap fst headers `Hspec.shouldContain` [Header.xssProtection]

    Hspec.it "does not overwrite an existing header" $ do
      headers <- getHeaders headerApplication
      headers `Hspec.shouldContain` [(Header.contentSecurityPolicy, "custom-csp")]

getHeaders :: Wai.Application -> IO Http.ResponseHeaders
getHeaders application =
  Wai.Test.withSession
    (AddSecurityHeaders.middleware application)
    . fmap Wai.Test.simpleHeaders
    $ Wai.Test.request Wai.Test.defaultRequest

emptyApplication :: Wai.Application
emptyApplication _ respond = respond $ Wai.responseLBS Http.status200 [] ""

headerApplication :: Wai.Application
headerApplication _ respond = respond $ Wai.responseLBS Http.status200 [(Header.contentSecurityPolicy, "custom-csp")] ""
