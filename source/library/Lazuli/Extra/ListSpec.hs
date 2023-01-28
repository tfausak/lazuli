module Lazuli.Extra.ListSpec where

import qualified Lazuli.Extra.List as List
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Lazuli.Extra.List" $ do
  Hspec.describe "appendIfMissing" $ do
    Hspec.it "works with an empty list" $ do
      List.appendIfMissing ('k', True) [] `Hspec.shouldBe` [('k', True)]

    Hspec.it "appends to the end" $ do
      List.appendIfMissing ('k', True) [('a', False)] `Hspec.shouldBe` [('a', False), ('k', True)]

    Hspec.it "does not overwrite existing value" $ do
      List.appendIfMissing ('k', True) [('k', False)] `Hspec.shouldBe` [('k', False)]

  Hspec.describe "appendIfMissingBy" $ do
    Hspec.it "works with an empty list" $ do
      List.appendIfMissingBy (==) 'k' "" `Hspec.shouldBe` "k"

    Hspec.it "appends to the end" $ do
      List.appendIfMissingBy (==) 'k' "a" `Hspec.shouldBe` "ak"

    Hspec.it "does not overwrite existing value" $ do
      List.appendIfMissingBy (==) 'k' "k" `Hspec.shouldBe` "k"
