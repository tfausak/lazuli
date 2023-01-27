module Lazuli where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
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
  mapM_ (Exception.throwIO . UnexpectedArgument.UnexpectedArgument) unexpectedArguments
  mapM_ (Exception.throwIO . UnknownOption.UnknownOption) unknownOptions
  mapM_ (Exception.throwIO . InvalidOption.InvalidOption) invalidOptions
  config <- Monad.foldM Config.applyFlag Config.initial flags
  Monad.when (Config.help config) $ do
    name <- Environment.getProgName
    putStr $ GetOpt.usageInfo name Flag.optDescrs
    Exit.exitSuccess

testSuite :: IO ()
testSuite = pure ()
