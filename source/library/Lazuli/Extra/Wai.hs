module Lazuli.Extra.Wai where

import qualified Network.Wai as Wai

type ApplicationWith m =
  Wai.Request ->
  (Wai.Response -> m Wai.ResponseReceived) ->
  m Wai.ResponseReceived

type MiddlewareWith m = ApplicationWith m -> ApplicationWith m
