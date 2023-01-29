module Lazuli.Action.Exception.HandleSpec where

import qualified Data.IORef as IORef
import qualified Lazuli.Action.Exception.Handle as Exception.Handle
import qualified Lazuli.Exception.TestError as TestError
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Action.Exception.Handle" $ do
  Hspec.describe "runWith" $ do
    Hspec.it "works" $ do
      ref <- IORef.newIORef ""
      Exception.Handle.runWith (IORef.writeIORef ref) TestError.TestError
      IORef.readIORef ref `Hspec.shouldReturn` "TestError"
