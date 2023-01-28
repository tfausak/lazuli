module Lazuli.Main.Executable where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString as ByteString
import Data.Function ((&))
import qualified GHC.Conc as Conc
import qualified Lazuli.Action.Config.Load as Config.Load
import qualified Lazuli.Action.Context.Load as Context.Load
import qualified Lazuli.Constant.Version as Version
import qualified Lazuli.Server.Application as Application
import qualified Lazuli.Server.Middleware as Middleware
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Context as Context
import qualified Lazuli.Type.Flag as Flag
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Witch

executable :: IO ()
executable = do
  handler <- Conc.getUncaughtExceptionHandler
  Conc.setUncaughtExceptionHandler $ Catch.handle handler . uncaughtExceptionHandler

  config <- Config.Load.run

  Monad.when (Config.help config) $ do
    name <- Environment.getProgName
    putStr $ GetOpt.usageInfo (name <> " version " <> Version.string) Flag.optDescrs
    Exit.exitSuccess

  Monad.when (Config.version config) $ do
    putStrLn Version.string
    Exit.exitSuccess

  context <- Context.Load.run config
  Warp.runSettings (settings $ Context.config context) $ Middleware.middleware (Context.requestIdKey context) Application.application

uncaughtExceptionHandler :: Catch.SomeException -> IO ()
uncaughtExceptionHandler (Catch.SomeException e) =
  IO.hPutStrLn IO.stderr $ "lazuli-" <> Version.string <> ": " <> Catch.displayException e

settings :: Config.Config -> Warp.Settings
settings config =
  Warp.defaultSettings
    & Warp.setBeforeMainLoop (putStrLn $ "Listening on " <> show (Config.port config) <> " ...")
    & Warp.setGracefulShutdownTimeout (Just 30)
    & Warp.setOnException (const uncaughtExceptionHandler)
    & Warp.setPort (Witch.into @Warp.Port $ Config.port config)
    & Warp.setServerName ByteString.empty
