module Lazuli.Extra.GetOpt where

import qualified Control.Monad.Catch as Catch
import qualified Data.Maybe as Maybe
import qualified Data.Traversable as Traversable
import qualified Lazuli.Exception.InvalidOption as InvalidOption
import qualified Lazuli.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Lazuli.Exception.UnknownOption as UnknownOption
import qualified System.Console.GetOpt as GetOpt

fromArguments :: (Catch.MonadThrow m) => [GetOpt.OptDescr a] -> [String] -> m [a]
fromArguments optDescrs arguments = do
  let (xs, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute optDescrs arguments
  mapM_ (Catch.throwM . UnexpectedArgument.UnexpectedArgument) args
  mapM_ (Catch.throwM . UnknownOption.UnknownOption) opts
  mapM_ (Catch.throwM . InvalidOption.InvalidOption) errs
  pure xs

fromEnvironment ::
  (Applicative m) =>
  [GetOpt.OptDescr a] ->
  (String -> String) ->
  (String -> m (Maybe String)) ->
  m [a]
fromEnvironment optDescrs modify lookupEnv =
  fmap concat . Traversable.for optDescrs $ \(GetOpt.Option _ strings argDescr _) ->
    fmap Maybe.catMaybes . Traversable.for strings $ \string -> do
      maybeValue <- lookupEnv $ modify string
      pure $ case argDescr of
        GetOpt.NoArg x -> fmap (const x) maybeValue
        GetOpt.ReqArg f _ -> fmap f maybeValue
        GetOpt.OptArg f _ -> fmap (f . Just) maybeValue
