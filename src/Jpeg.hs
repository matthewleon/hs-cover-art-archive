{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Jpeg where

import Servant.API.ContentTypes (Accept(..), MimeUnrender(..))
import Network.HTTP.Media.MediaType ((//))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, toStrict)

data JPEGContent = JPEGContent
  deriving (Show)
instance Accept JPEGContent where
  contentType _ = "image" // "jpeg"
instance MimeUnrender JPEGContent JPEG where
  mimeUnrender _ = Right . JPEG . toStrict
instance MimeUnrender JPEGContent ByteString where
  mimeUnrender _ = Right
instance MimeUnrender JPEGContent BS.ByteString where
  mimeUnrender _ = Right . toStrict

newtype JPEG = JPEG BS.ByteString
  deriving (Show)
