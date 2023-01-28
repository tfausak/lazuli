module Lazuli.Type.RequestId where

import qualified Data.Word as Word
import qualified System.Random as Random
import qualified Text.Printf as Printf
import qualified Witch

newtype RequestId
  = RequestId Word.Word32
  deriving (Eq, Show)
  deriving (Random.Random) via Word.Word32

instance Witch.From RequestId Word.Word32

instance Witch.From RequestId String where
  from = Printf.printf "%08x" . Witch.into @Word.Word32
