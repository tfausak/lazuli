module LazuliSpec where

import qualified Lazuli.Action.Config.LoadSpec
import qualified Lazuli.Action.Context.LoadSpec
import qualified Lazuli.Action.Exception.HandleSpec
import qualified Lazuli.Extra.GetOptSpec
import qualified Lazuli.Extra.HspecSpec
import qualified Lazuli.Extra.ListSpec
import qualified Lazuli.Middleware.AddRequestIdSpec
import qualified Lazuli.Middleware.AddSecurityHeadersSpec
import qualified Lazuli.Middleware.HandleExceptionsSpec
import qualified Lazuli.Middleware.LogResponsesSpec
import qualified Lazuli.Type.ConfigSpec
import qualified Lazuli.Type.PortSpec
import qualified Lazuli.Type.RequestIdSpec
import qualified Lazuli.Type.UrlSpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do
  Lazuli.Action.Config.LoadSpec.spec
  Lazuli.Action.Context.LoadSpec.spec
  Lazuli.Action.Exception.HandleSpec.spec
  Lazuli.Extra.GetOptSpec.spec
  Lazuli.Extra.HspecSpec.spec
  Lazuli.Extra.ListSpec.spec
  Lazuli.Middleware.AddRequestIdSpec.spec
  Lazuli.Middleware.AddSecurityHeadersSpec.spec
  Lazuli.Middleware.HandleExceptionsSpec.spec
  Lazuli.Middleware.LogResponsesSpec.spec
  Lazuli.Type.ConfigSpec.spec
  Lazuli.Type.PortSpec.spec
  Lazuli.Type.RequestIdSpec.spec
  Lazuli.Type.UrlSpec.spec
