module Lazuli.Type.Flag where

import qualified System.Console.GetOpt as GetOpt

data Flag
  = Help Bool
  | Port String
  | SentryDsn String
  | Version Bool
  deriving (Eq, Show)

optDescrs :: [GetOpt.OptDescr Flag]
optDescrs =
  [ GetOpt.Option
      ['h', '?']
      ["help"]
      (GetOpt.NoArg $ Help True)
      "Shows this help message, then exits.",
    GetOpt.Option
      []
      ["no-help"]
      (GetOpt.NoArg $ Help False)
      "",
    GetOpt.Option
      []
      ["version"]
      (GetOpt.NoArg $ Version True)
      "Shows the version number, then exits.",
    GetOpt.Option
      []
      ["no-version"]
      (GetOpt.NoArg $ Version False)
      "",
    GetOpt.Option
      []
      ["port"]
      (GetOpt.ReqArg Port "NUMBER")
      "Sets the port to listen on.",
    GetOpt.Option
      []
      ["sentry-dsn"]
      (GetOpt.ReqArg SentryDsn "URL")
      "Sets the Sentry DSN for exception reporting."
  ]
