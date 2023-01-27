module Lazuli.Type.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Lazuli.Exception.InvalidOption as InvalidOption
import qualified Lazuli.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Lazuli.Exception.UnknownOption as UnknownOption
import qualified Lazuli.Type.Flag as Flag
import qualified System.Console.GetOpt as GetOpt

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

fromArguments :: (Catch.MonadThrow m) => [String] -> m Config
fromArguments arguments = do
  let (flags, unexpectedArguments, unknownOptions, invalidOptions) =
        GetOpt.getOpt' GetOpt.Permute Flag.optDescrs arguments
  mapM_ (Catch.throwM . UnexpectedArgument.UnexpectedArgument) unexpectedArguments
  mapM_ (Catch.throwM . UnknownOption.UnknownOption) unknownOptions
  mapM_ (Catch.throwM . InvalidOption.InvalidOption) invalidOptions
  Monad.foldM applyFlag initial flags
