module Lazuli.Main.Executable where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString as ByteString
import Data.Function ((&))
import qualified GHC.Conc as Conc
import qualified Lazuli.Action.Config.Load as Config.Load
import qualified Lazuli.Action.Context.Load as Context.Load
import qualified Lazuli.Action.Exception.Handle as Exception.Handle
import qualified Lazuli.Constant.Version as Version
import qualified Lazuli.Log as Log
import qualified Lazuli.Server.Application as Application
import qualified Lazuli.Server.Middleware as Middleware
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Context as Context
import qualified Lazuli.Type.Flag as Flag
import qualified Network.HTTP.Types as Http
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified Witch

executable :: IO ()
executable = do
  config <- Config.Load.run

  Monad.when (Config.help config) $ do
    name <- Environment.getProgName
    let header =
          unlines
            [ name <> " version " <> Version.string,
              "https://github.com/tfausak/lazuli",
              "",
              "All flags can also be set with environment variables. For \
              \example, the '--data-directory=PATH' flag can be set with the \
              \'LAZULI_DATA_DIRECTORY=PATH' environment variable. If both an \
              \environment variable and a flag are set, the flag takes \
              \precedence. Flags are processed in order, and later flags \
              \override earlier flags."
            ]
    putStr $ GetOpt.usageInfo header Flag.optDescrs
    Exit.exitSuccess

  Monad.when (Config.version config) $ do
    putStrLn Version.string
    Exit.exitSuccess

  context <- Context.Load.run config

  handler <- Conc.getUncaughtExceptionHandler
  Conc.setUncaughtExceptionHandler $ Catch.handle handler . Exception.Handle.run context Nothing

  Warp.runSettings (settings context)
    . Middleware.middleware context
    $ Application.application context

settings :: Context.Context -> Warp.Settings
settings context =
  let host = Config.host $ Context.config context
      port = Config.port $ Context.config context
   in Warp.defaultSettings
        & Warp.setBeforeMainLoop (Log.info context $ "Listening on " <> show host <> " " <> show port <> " ...")
        & Warp.setGracefulShutdownTimeout (Just 30)
        & Warp.setHost host
        & Warp.setOnException (Exception.Handle.run context)
        & Warp.setOnExceptionResponse (const $ Application.statusResponse Http.internalServerError500 [])
        & Warp.setPort (Witch.into @Warp.Port port)
        & Warp.setServerName ByteString.empty
