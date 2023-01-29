module Lazuli.Action.Exception.Handle where

import qualified Control.Monad.Catch as Catch
import qualified System.IO as IO

run :: (Catch.Exception e) => e -> IO ()
run = runWith $ IO.hPutStrLn IO.stderr

runWith :: (Catch.Exception e) => (String -> m ()) -> e -> m ()
runWith myPutStrLn exception =
  myPutStrLn $ Catch.displayException exception
