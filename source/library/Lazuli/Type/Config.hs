module Lazuli.Type.Config where

import qualified Control.Monad.Catch as Catch
import qualified Lazuli.Type.Flag as Flag
import qualified Lazuli.Type.Port as Port
import qualified Witch

data Config = Config
  { help :: Bool,
    port :: Port.Port,
    version :: Bool
  }
  deriving (Eq, Show)

initial :: Config
initial =
  Config
    { help = False,
      port = Port.Port 3000,
      version = False
    }

applyFlag :: (Catch.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Help h -> pure config {help = h}
  Flag.Port s -> case Witch.tryFrom s of
    Left e -> Catch.throwM e
    Right p -> pure config {port = p}
  Flag.Version v -> pure config {version = v}
