module Lazuli where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Lazuli.Exception.InvalidOption as InvalidOption
import qualified Lazuli.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Lazuli.Exception.UnknownOption as UnknownOption
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Flag as Flag
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit

executable :: IO ()
executable = do
  arguments <- Environment.getArgs
  let (flags, unexpectedArguments, unknownOptions, invalidOptions) =
        GetOpt.getOpt' GetOpt.Permute Flag.optDescrs arguments
  mapM_ (Catch.throwM . UnexpectedArgument.UnexpectedArgument) unexpectedArguments
  mapM_ (Catch.throwM . UnknownOption.UnknownOption) unknownOptions
  mapM_ (Catch.throwM . InvalidOption.InvalidOption) invalidOptions
  config <- Monad.foldM Config.applyFlag Config.initial flags
  Monad.when (Config.help config) $ do
    name <- Environment.getProgName
    putStr $ GetOpt.usageInfo name Flag.optDescrs
    Exit.exitSuccess

testSuite :: IO ()
testSuite = pure ()