module Lazuli.Type.Flag where

import qualified System.Console.GetOpt as GetOpt

data Flag
  = BaseUrl String
  | Commit String
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
      "Prints this help message, then exits successfully.",
    GetOpt.Option
      []
      ["no-help"]
      (GetOpt.NoArg $ Help False)
      "Prevents this help message from being printed.",
    GetOpt.Option
      []
      ["version"]
      (GetOpt.NoArg $ Version True)
      "Prints the version number, then exits successfully.",
    GetOpt.Option
      []
      ["no-version"]
      (GetOpt.NoArg $ Version False)
      "Prevents the version number from being printed.",
    GetOpt.Option
      []
      ["base-url"]
      (GetOpt.ReqArg BaseUrl "URL")
      "Sets the base URL for generating links. Defaults to '/'. Can be either \
      \a path or URL. A trailing slash will be added if missing.",
    GetOpt.Option
      []
      ["commit"]
      (GetOpt.ReqArg Commit "HASH")
      "Sets the commit hash for diagnostics. Does not have a default value.",
    GetOpt.Option
      []
      ["data-directory"]
      (GetOpt.ReqArg DataDirectory "PATH")
      "Sets the directory to find data files in. Defaults to 'data'.",
    GetOpt.Option
      []
      ["environment"]
      (GetOpt.ReqArg Environment "NAME")
      "Sets the environment to run in. Defaults to 'development'. Must be one \
      \of 'development', 'testing', or 'production'.",
    GetOpt.Option
      []
      ["host"]
      (GetOpt.ReqArg Host "PREFERENCE")
      "Sets the host to listen on. Defaults to '127.0.0.1'. Can use the \
      \special value '*' to listen on all hosts.",
    GetOpt.Option
      []
      ["port"]
      (GetOpt.ReqArg Port "NUMBER")
      "Sets the port to listen on. Defaults to '3000'.",
    GetOpt.Option
      []
      ["sentry-dsn"]
      (GetOpt.ReqArg SentryDsn "URL")
      "Sets the Sentry DSN for exception reporting. Does not have a default \
      \value."
  ]
