module Lazuli.Type.RequestId where

import qualified Data.Word as Word
import qualified System.Random as Random

newtype RequestId
  = RequestId Word.Word64
  deriving (Eq, Show)
  deriving (Random.Random) via Word.Word64

intoWord64 :: RequestId -> Word.Word64
intoWord64 (RequestId x) = x
