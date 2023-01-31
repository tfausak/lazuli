module Lazuli.Constant.Mime where

import qualified Data.ByteString as ByteString

imageIcon :: ByteString.ByteString
imageIcon = "image/x-icon"

textHtml :: ByteString.ByteString
textHtml = "text/html;charset=utf-8"

textPlain :: ByteString.ByteString
textPlain = "text/plain;charset=utf-8"
