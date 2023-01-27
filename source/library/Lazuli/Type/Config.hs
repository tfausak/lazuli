module Lazuli.Type.Config where

import qualified Control.Monad.Catch as Catch
import qualified Lazuli.Type.Flag as Flag

data Config = Config
  { help :: Bool,
    version :: Bool
  }
  deriving (Eq, Show)

initial :: Config
initial =
  Config
    { help = False,
      version = False
    }

applyFlag :: (Catch.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Help -> pure config {help = True}
  Flag.Version -> pure config {version = True}
