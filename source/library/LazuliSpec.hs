module LazuliSpec where

import qualified Lazuli.Type.ConfigSpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = do
  Lazuli.Type.ConfigSpec.spec
