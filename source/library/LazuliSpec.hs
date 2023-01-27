module LazuliSpec where

import qualified Lazuli.Type.ConfigSpec
import qualified Lazuli.Type.PortSpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do
  Lazuli.Type.ConfigSpec.spec
  Lazuli.Type.PortSpec.spec
