module Lazuli.Type.Flag where

import qualified System.Console.GetOpt as GetOpt

data Flag
  = Help
  | Port String
  | Version
  deriving (Eq, Show)

optDescrs :: [GetOpt.OptDescr Flag]
optDescrs =
  [ GetOpt.Option
      ['h', '?']
      ["help"]
      (GetOpt.NoArg Help)
      "Shows this help message, then exits.",
    GetOpt.Option
      []
      ["version"]
      (GetOpt.NoArg Version)
      "Shows the version number, then exits.",
    GetOpt.Option
      []
      ["port"]
      (GetOpt.ReqArg Port "NUMBER")
      "Sets the port to listen on."
  ]
