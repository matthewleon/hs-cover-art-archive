{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module CoverArtArchive.Internal
    ( getListing
    , getListingArt
    , getFront
    , getFront250
    , getFront500
    , getBack
    , getBack250
    , getBack500
    , getRelease
    , getReleaseFront
    , getReleaseFront250
    , getReleaseFront500
    ) where

import Data.Proxy (Proxy(..))
import Servant.API (JSON, Capture, Get, (:<|>)(..), (:>))
import Servant.Client (ClientM, client)

import CoverArtArchive.Types
import Jpeg (JPEGContent, JPEG)

type API = "release" :> Capture "mbid" Mbid :> Get '[JSON] Listing
      :<|> "release" :> Capture "mbid" Mbid :> Capture "fileid" FileID :> Get '[JPEGContent] JPEG
      :<|> "release" :> Capture "mbid" Mbid :> "front" :> Get '[JPEGContent] JPEG
      :<|> "release" :> Capture "mbid" Mbid :> "front-250" :> Get '[JPEGContent] JPEG
      :<|> "release" :> Capture "mbid" Mbid :> "front-500" :> Get '[JPEGContent] JPEG
      :<|> "release" :> Capture "mbid" Mbid :> "back" :> Get '[JPEGContent] JPEG
      :<|> "release" :> Capture "mbid" Mbid :> "back-250" :> Get '[JPEGContent] JPEG
      :<|> "release" :> Capture "mbid" Mbid :> "back-500" :> Get '[JPEGContent] JPEG
      :<|> "release-group" :> Capture "mbid" Mbid :> Get '[JSON] Listing
      :<|> "release-group" :> Capture "mbid" Mbid :> "front" :> Get '[JPEGContent] JPEG
      :<|> "release-group" :> Capture "mbid" Mbid :> "front-250" :> Get '[JPEGContent] JPEG
      :<|> "release-group" :> Capture "mbid" Mbid :> "front-500" :> Get '[JPEGContent] JPEG

coverArtArchiveAPI :: Proxy API
coverArtArchiveAPI  = Proxy

getListing         :: Mbid -> ClientM Listing
getListingArt      :: Mbid -> FileID -> ClientM JPEG
getFront           :: Mbid -> ClientM JPEG
getFront250        :: Mbid -> ClientM JPEG
getFront500        :: Mbid -> ClientM JPEG
getBack            :: Mbid -> ClientM JPEG
getBack250         :: Mbid -> ClientM JPEG
getBack500         :: Mbid -> ClientM JPEG
getRelease         :: Mbid -> ClientM Listing
getReleaseFront    :: Mbid -> ClientM JPEG
getReleaseFront250 :: Mbid -> ClientM JPEG
getReleaseFront500 :: Mbid -> ClientM JPEG
getListing
  :<|> getListingArt
  :<|> getFront
  :<|> getFront250
  :<|> getFront500
  :<|> getBack
  :<|> getBack250
  :<|> getBack500
  :<|> getRelease
  :<|> getReleaseFront
  :<|> getReleaseFront250
  :<|> getReleaseFront500 = client coverArtArchiveAPI
