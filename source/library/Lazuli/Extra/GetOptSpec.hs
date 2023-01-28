module Lazuli.Extra.GetOptSpec where

import qualified Lazuli.Exception.InvalidOption as InvalidOption
import qualified Lazuli.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Lazuli.Exception.UnknownOption as UnknownOption
import qualified Lazuli.Extra.GetOpt as GetOpt
import qualified Lazuli.Extra.Hspec as Hspec
import qualified System.Console.GetOpt as GetOpt
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Extra.GetOpt" $ do
  Hspec.describe "fromArguments" $ do
    Hspec.it "works with no arguments" $ do
      GetOpt.fromArguments optDescrs [] `Hspec.shouldReturn` []

    Hspec.it "fails with an unexpected argument" $ do
      GetOpt.fromArguments optDescrs ["unexpected"] `Hspec.shouldThrow` Hspec.exceptionType @UnexpectedArgument.UnexpectedArgument

    Hspec.it "fails with an unknown option" $ do
      GetOpt.fromArguments optDescrs ["--unknown"] `Hspec.shouldThrow` Hspec.exceptionType @UnknownOption.UnknownOption

    Hspec.it "fails with an invalid option" $ do
      GetOpt.fromArguments optDescrs ["--help=invalid"] `Hspec.shouldThrow` Hspec.exceptionType @InvalidOption.InvalidOption

    Hspec.it "works with an argument" $ do
      GetOpt.fromArguments optDescrs ["--help"] `Hspec.shouldReturn` [Just "help"]

  Hspec.describe "fromEnvironment" $ do
    Hspec.it "works with no environment variables" $ do
      GetOpt.fromEnvironment optDescrs id (pure . flip lookup []) `Hspec.shouldReturn` []

    Hspec.it "works with a no-argument environment variable" $ do
      GetOpt.fromEnvironment optDescrs id (pure . flip lookup [("no-arg", "x")]) `Hspec.shouldReturn` [Just "no-arg"]

    Hspec.it "works with a required-argument environment variable" $ do
      GetOpt.fromEnvironment optDescrs id (pure . flip lookup [("req-arg", "x")]) `Hspec.shouldReturn` [Just "x"]

    Hspec.it "works with an optional-argument environment variable" $ do
      GetOpt.fromEnvironment optDescrs id (pure . flip lookup [("opt-arg", "x")]) `Hspec.shouldReturn` [Just "x"]

    Hspec.it "works with a modified string" $ do
      GetOpt.fromEnvironment optDescrs reverse (pure . flip lookup [("gra-on", "x")]) `Hspec.shouldReturn` [Just "no-arg"]

optDescrs :: [GetOpt.OptDescr (Maybe String)]
optDescrs =
  [ GetOpt.Option ['h'] ["help"] (GetOpt.NoArg $ Just "help") "",
    GetOpt.Option [] ["no-arg"] (GetOpt.NoArg $ Just "no-arg") "",
    GetOpt.Option [] ["req-arg"] (GetOpt.ReqArg Just "") "",
    GetOpt.Option [] ["opt-arg"] (GetOpt.OptArg id "") ""
  ]
