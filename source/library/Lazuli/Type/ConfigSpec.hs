module Lazuli.Type.ConfigSpec where

import qualified Lazuli.Extra.Either as Either
import qualified Lazuli.Extra.Hspec as Hspec
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Environment as Environment
import qualified Lazuli.Type.Flag as Flag
import qualified Lazuli.Type.LogLevel as LogLevel
import qualified Lazuli.Type.Port as Port
import qualified Lazuli.Type.Url as Url
import qualified Patrol
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Type.Config" $ do
  Hspec.describe "applyFlag" $ do
    let config = Config.testing

    Hspec.it "handles the help flag" $ do
      Config.applyFlag config (Flag.Help True) `Hspec.shouldReturn` config {Config.help = True}

    Hspec.it "handles the version flag" $ do
      Config.applyFlag config (Flag.Version True) `Hspec.shouldReturn` config {Config.version = True}

    Hspec.it "handles the base URL flag" $ do
      Config.applyFlag config (Flag.BaseUrl "http://test/") `Hspec.shouldReturn` config {Config.baseUrl = "http://test/"}

    Hspec.it "ensures a trailing slash on the base URL" $ do
      Config.applyFlag config (Flag.BaseUrl "http://test") `Hspec.shouldReturn` config {Config.baseUrl = "http://test/"}

    Hspec.it "handles the commit flag" $ do
      Config.applyFlag config (Flag.Commit "01ef") `Hspec.shouldReturn` config {Config.commit = Just "01ef"}

    Hspec.it "handles the data directory flag" $ do
      Config.applyFlag config (Flag.DataDirectory "datum") `Hspec.shouldReturn` config {Config.dataDirectory = "datum"}

    Hspec.it "sets a valid environment" $ do
      Config.applyFlag config (Flag.Environment "testing") `Hspec.shouldReturn` config {Config.environment = Environment.Testing}

    Hspec.it "rejects an invalid environment" $ do
      Config.applyFlag config (Flag.Environment "invalid") `Hspec.shouldThrow` Hspec.exceptionType @(Witch.TryFromException String Environment.Environment)

    Hspec.it "handles the host flag" $ do
      Config.applyFlag config (Flag.Host "*") `Hspec.shouldReturn` config {Config.host = "*"}

    Hspec.it "sets a valid log level" $ do
      Config.applyFlag config (Flag.LogLevel "info") `Hspec.shouldReturn` config {Config.logLevel = LogLevel.Info}

    Hspec.it "rejects an invalid log level" $ do
      Config.applyFlag config (Flag.LogLevel "invalid") `Hspec.shouldThrow` Hspec.exceptionType @(Witch.TryFromException String LogLevel.LogLevel)

    Hspec.it "sets a valid port" $ do
      Config.applyFlag config (Flag.Port "1234") `Hspec.shouldReturn` config {Config.port = Port.Port 1234}

    Hspec.it "rejects an invalid port" $ do
      Config.applyFlag config (Flag.Port "invalid") `Hspec.shouldThrow` Hspec.exceptionType @(Witch.TryFromException String Port.Port)

    Hspec.it "sets a valid Sentry DSN" $ do
      let string = "http://user@test" :: String
      url <- Either.throw $ Witch.tryInto @Url.Url string
      dsn <- Either.throw $ Witch.tryInto @Patrol.Dsn url
      Config.applyFlag config (Flag.SentryDsn string) `Hspec.shouldReturn` config {Config.sentryDsn = Just dsn}

    Hspec.it "rejects an invalid Sentry DSN" $ do
      Config.applyFlag config (Flag.SentryDsn "invalid") `Hspec.shouldThrow` Hspec.exceptionType @(Witch.TryFromException String Patrol.Dsn)
