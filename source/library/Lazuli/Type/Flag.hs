module Lazuli.Type.Flag where

import qualified System.Console.GetOpt as GetOpt

data Flag
  = Commit String
  | DataDirectory FilePath
  | Environment String
  | Help Bool
  | Host String
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
      ["commit"]
      (GetOpt.ReqArg Commit "HASH")
      "Sets the commit hash for diagnostics. No default.",
    GetOpt.Option
      []
      ["data-directory"]
      (GetOpt.ReqArg DataDirectory "PATH")
      "Sets the directory to find data files in. Defaults to data.",
    GetOpt.Option
      []
      ["environment"]
      (GetOpt.ReqArg Environment "NAME")
      "Sets the environment to run in. Defaults to development.",
    GetOpt.Option
      []
      ["host"]
      (GetOpt.ReqArg Host "PREFERENCE")
      "Sets the host to listen on. Defaults to 127.0.0.1.",
    GetOpt.Option
      []
      ["port"]
      (GetOpt.ReqArg Port "NUMBER")
      "Sets the port to listen on. Defaults to 3000.",
    GetOpt.Option
      []
      ["sentry-dsn"]
      (GetOpt.ReqArg SentryDsn "URL")
      "Sets the Sentry DSN for exception reporting. No default."
  ]
