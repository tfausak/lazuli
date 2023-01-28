module Lazuli.Type.RequestId where

import qualified Data.Word as Word
import qualified System.Random as Random
import qualified Witch

newtype RequestId
  = RequestId Word.Word64
  deriving (Eq, Show)
  deriving (Random.Random) via Word.Word64

instance Witch.From RequestId Word.Word64
