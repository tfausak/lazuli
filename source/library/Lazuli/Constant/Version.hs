module Lazuli.Constant.Version where

import qualified Data.Version as Version
import qualified Paths_lazuli as Lazuli

string :: String
string = Version.showVersion version

version :: Version.Version
version = Lazuli.version
