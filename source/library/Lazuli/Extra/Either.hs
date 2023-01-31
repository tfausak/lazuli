module Lazuli.Extra.Either where

import qualified Control.Monad.Catch as Catch

hush :: Either x a -> Maybe a
hush = either (const Nothing) Just

throw :: (Catch.Exception e, Catch.MonadThrow m) => Either e a -> m a
throw = either Catch.throwM pure
