module Lazuli.Type.Url where

import qualified Network.URI as Uri
import qualified Patrol
import qualified Patrol.Type.Dsn
import qualified Witch

newtype Url
  = Url Uri.URI
  deriving (Eq, Show)

instance Witch.From Uri.URI Url

instance Witch.From Url Uri.URI

instance Witch.TryFrom String Url where
  tryFrom = Witch.maybeTryFrom $ fmap Witch.from . Uri.parseURI

instance Witch.TryFrom Url Patrol.Dsn where
  tryFrom = Witch.eitherTryFrom $ Patrol.Type.Dsn.fromUri . Witch.from
