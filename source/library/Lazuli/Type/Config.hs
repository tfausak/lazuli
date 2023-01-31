module Lazuli.Type.Config where

import qualified Control.Monad.Catch as Catch
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Lazuli.Type.Environment as Environment
import qualified Lazuli.Type.Flag as Flag
import qualified Lazuli.Type.Port as Port
import qualified Lazuli.Type.Url as Url
import qualified Network.Wai.Handler.Warp as Warp
import qualified Patrol
import qualified Witch

data Config = Config
  { commit :: Maybe Text.Text,
    dataDirectory :: FilePath,
    environment :: Environment.Environment,
    help :: Bool,
    host :: Warp.HostPreference,
    port :: Port.Port,
    sentryDsn :: Maybe Patrol.Dsn,
    version :: Bool
  }
  deriving (Eq, Show)

development :: Config
development =
  Config
    { commit = Nothing,
      dataDirectory = "data",
      environment = Environment.Development,
      help = False,
      host = "127.0.0.1",
      port = Port.Port 3000,
      sentryDsn = Nothing,
      version = False
    }

testing :: Config
testing = development {environment = Environment.Testing}

applyFlag :: (Catch.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Commit s -> pure config {commit = Just $ Witch.from s}
  Flag.DataDirectory s -> pure config {dataDirectory = s}
  Flag.Environment s -> case Witch.tryFrom s of
    Left e -> Catch.throwM e
    Right e -> pure config {environment = e}
  Flag.Help h -> pure config {help = h}
  Flag.Host s -> pure config {host = String.fromString s}
  Flag.Port s -> case Witch.tryFrom s of
    Left e -> Catch.throwM e
    Right p -> pure config {port = p}
  Flag.SentryDsn s -> case Witch.tryVia @Url.Url s of
    Left e -> Catch.throwM e
    Right d -> pure config {sentryDsn = Just d}
  Flag.Version v -> pure config {version = v}
