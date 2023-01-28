module Lazuli.Action.Config.Load where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.Char as Char
import qualified Lazuli.Extra.GetOpt as GetOpt
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Flag as Flag
import qualified System.Environment as Environment

run :: IO Config.Config
run = do
  arguments <- Environment.getArgs
  runWith Environment.lookupEnv arguments

runWith ::
  (Catch.MonadThrow m) =>
  (String -> m (Maybe String)) ->
  [String] ->
  m Config.Config
runWith lookupEnv arguments = do
  let modify :: String -> String
      modify = mappend "LAZULI_" . fmap (\c -> if c == '-' then '_' else Char.toUpper c)
  environmentFlags <- GetOpt.fromEnvironment Flag.optDescrs modify lookupEnv
  argumentFlags <- GetOpt.fromArguments Flag.optDescrs arguments
  Monad.foldM Config.applyFlag Config.initial $ environmentFlags <> argumentFlags
