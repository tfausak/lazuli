module Lazuli.Main.Executable where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified GHC.Conc as Conc
import qualified Lazuli.Constant.Header as Header
import qualified Lazuli.Constant.Version as Version
import qualified Lazuli.Type.Config as Config
import qualified Lazuli.Type.Flag as Flag
import qualified Lucid
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
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

  Warp.runSettings Warp.defaultSettings $ \_request respond ->
    respond
      . Wai.responseLBS
        Http.ok200
        [ (Header.contentSecurityPolicy, "default-src 'none'"),
          (Http.hContentType, "text/html;charset=utf-8"),
          (Header.contentTypeOptions, "nosniff"),
          (Header.frameOptions, "DENY"),
          (Header.referrerPolicy, "no-referrer"),
          (Header.strictTransportSecurity, "max-age=31536000; includeSubDomains")
        ]
      . Lucid.renderBS
      $ do
        Lucid.doctype_
        Lucid.html_ [Lucid.lang_ "en-US"] $ do
          Lucid.head_ $ do
            Lucid.meta_ [Lucid.charset_ "utf-8"]
            Lucid.title_ "Lazuli"
          Lucid.body_ $ do
            Lucid.h1_ "Lazuli"

uncaughtExceptionHandler :: Catch.SomeException -> IO ()
uncaughtExceptionHandler (Catch.SomeException e) =
  IO.hPutStrLn IO.stderr $ "lazuli-" <> Version.string <> ": " <> Catch.displayException e
