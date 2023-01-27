module Lazuli.Main.Executable where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified GHC.Conc as Conc
import qualified Lazuli.Constant.Version as Version
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
  config <- Config.fromArguments arguments

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
