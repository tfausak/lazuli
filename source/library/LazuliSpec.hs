module LazuliSpec where

import qualified Lazuli.Action.Config.LoadSpec
import qualified Lazuli.Action.Context.LoadSpec
import qualified Lazuli.Extra.GetOptSpec
import qualified Lazuli.Extra.HspecSpec
import qualified Lazuli.Extra.ListSpec
import qualified Lazuli.Middleware.AddSecurityHeadersSpec
import qualified Lazuli.Type.ConfigSpec
import qualified Lazuli.Type.PortSpec
import qualified Lazuli.Type.RequestIdSpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do
  Lazuli.Action.Config.LoadSpec.spec
  Lazuli.Action.Context.LoadSpec.spec
  Lazuli.Extra.GetOptSpec.spec
  Lazuli.Extra.HspecSpec.spec
  Lazuli.Extra.ListSpec.spec
  Lazuli.Middleware.AddSecurityHeadersSpec.spec
  Lazuli.Type.ConfigSpec.spec
  Lazuli.Type.PortSpec.spec
  Lazuli.Type.RequestIdSpec.spec
