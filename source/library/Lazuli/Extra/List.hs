module Lazuli.Extra.List where

import qualified Data.Function as Function

appendIfMissing :: (Eq a) => (a, b) -> [(a, b)] -> [(a, b)]
appendIfMissing = appendIfMissingBy $ Function.on (==) fst

appendIfMissingBy :: (a -> a -> Bool) -> a -> [a] -> [a]
appendIfMissingBy p x xs = case xs of
  [] -> [x]
  h : t
    | p x h -> h : t
    | otherwise -> h : appendIfMissingBy p x t
