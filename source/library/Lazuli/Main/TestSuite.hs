module Lazuli.Main.TestSuite where

import qualified LazuliSpec
import qualified Test.Hspec as Hspec

testSuite :: IO ()
testSuite = Hspec.hspec LazuliSpec.spec
