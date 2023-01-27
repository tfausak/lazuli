module Lazuli.Type.Flag where

import qualified System.Console.GetOpt as GetOpt

data Flag
  = Help
  deriving (Eq, Show)

optDescrs :: [GetOpt.OptDescr Flag]
optDescrs =
  [ GetOpt.Option
      ['h', '?']
      ["help"]
      (GetOpt.NoArg Help)
      "Shows this help message, then exits."
  ]
