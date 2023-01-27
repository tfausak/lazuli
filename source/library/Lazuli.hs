module Lazuli where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified GHC.Conc as Conc
import qualified Lazuli.Constant.Version as Version
import qualified Lazuli.Exception.InvalidOption as InvalidOption
import qualified Lazuli.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Lazuli.Exception.UnknownOption as UnknownOption
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Flag as Flag
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

executable :: IO ()
executable = do
  handler <- Conc.getUncaughtExceptionHandler
  Conc.setUncaughtExceptionHandler $ Catch.handle handler . uncaughtExceptionHandler

  arguments <- Environment.getArgs
  let (flags, unexpectedArguments, unknownOptions, invalidOptions) =
        GetOpt.getOpt' GetOpt.Permute Flag.optDescrs arguments
  mapM_ (Catch.throwM . UnexpectedArgument.UnexpectedArgument) unexpectedArguments
  mapM_ (Catch.throwM . UnknownOption.UnknownOption) unknownOptions
  mapM_ (Catch.throwM . InvalidOption.InvalidOption) invalidOptions
  config <- Monad.foldM Config.applyFlag Config.initial flags
  Monad.when (Config.help config) $ do
    name <- Environment.getProgName
    putStr $ GetOpt.usageInfo (name <> " version " <> Version.string) Flag.optDescrs
    Exit.exitSuccess
  Monad.when (Config.version config) $ do
    putStrLn Version.string
    Exit.exitSuccess

uncaughtExceptionHandler :: Catch.SomeException -> IO ()
uncaughtExceptionHandler (Catch.SomeException e) =
  IO.hPutStrLn IO.stderr $ "lazuli-" <> Version.string <> ": " <> Catch.displayException e

testSuite :: IO ()
testSuite = pure ()
