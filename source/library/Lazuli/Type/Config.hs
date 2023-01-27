module Lazuli.Type.Config where

import qualified Lazuli.Type.Flag as Flag

newtype Config = Config
  { help :: Bool
  }
  deriving (Eq, Show)

initial :: Config
initial =
  Config
    { help = False
    }

applyFlag :: (Applicative m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.Help -> pure config {help = True}
