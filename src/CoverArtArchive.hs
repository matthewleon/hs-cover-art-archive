{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module CoverArtArchive
    ( getListing
    , getListingArt
    , getFront
    , getFront250
    , getFront500
    , getBack
    , getBack250
    , getBack500
    , getRelease
--    , getReleaseFront
--    , getReleaseFront250
--    , getReleaseFront500
    ) where

import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (ClientM, ClientEnv(..), BaseUrl(..), Scheme(Https), runClientM, ServantError)
import qualified Data.Bifunctor as Bifunctor

import qualified CoverArtArchive.Internal as Internal
import CoverArtArchive.Types
import CoverArtArchive.Errors
import Jpeg (JPEG)

defaultBaseUrl :: BaseUrl
defaultBaseUrl  = BaseUrl Https "coverartarchive.org" 443 ""

defaultClientEnv :: IO ClientEnv
defaultClientEnv  = do
  manager <- newTlsManager
  return $ ClientEnv manager defaultBaseUrl

getListing :: Mbid -> IO (Either (QueryError ListingError) Listing)
getListing  = withDefaultClientEnv getListing'

getListing' :: ClientEnv -> Mbid -> IO (Either (QueryError ListingError) Listing)
getListing' env mbid = fmap mapListingResult $ runClientM req env
    where req = Internal.getListing mbid

getListingArt :: Mbid -> FileID -> IO (Either (QueryError ImageError) JPEG)
getListingArt = withDefaultClientEnv2 getListingArt'

getListingArt' :: ClientEnv -> Mbid -> FileID -> IO (Either (QueryError ImageError) JPEG)
getListingArt' env mbid fid = fmap mapImageResult $ runClientM req env
    where req = Internal.getListingArt mbid fid

getRelease :: Mbid -> IO (Either (QueryError ListingError) Listing)
getRelease  = withDefaultClientEnv getRelease'

getRelease' :: ClientEnv -> Mbid -> IO (Either (QueryError ListingError) Listing)
getRelease' env mbid = fmap mapListingResult $ runClientM req env
    where req = Internal.getRelease mbid

getFront :: Mbid -> IO (Either (QueryError ImageError) JPEG)
getFront = withDefaultClientEnv getFront'

getFront' :: ClientEnv -> Mbid -> IO (Either (QueryError ImageError) JPEG)
getFront' = mkGetArt Internal.getFront

getFront250 :: Mbid -> IO (Either (QueryError ImageError) JPEG)
getFront250 = withDefaultClientEnv getFront250'

getFront250' :: ClientEnv -> Mbid -> IO (Either (QueryError ImageError) JPEG)
getFront250' = mkGetArt Internal.getFront250

getFront500 :: Mbid -> IO (Either (QueryError ImageError) JPEG)
getFront500 = withDefaultClientEnv getFront500'

getFront500' :: ClientEnv -> Mbid -> IO (Either (QueryError ImageError) JPEG)
getFront500' = mkGetArt Internal.getFront500

getBack :: Mbid -> IO (Either (QueryError ImageError) JPEG)
getBack = withDefaultClientEnv getBack'

getBack' :: ClientEnv -> Mbid -> IO (Either (QueryError ImageError) JPEG)
getBack' = mkGetArt Internal.getBack

getBack250 :: Mbid -> IO (Either (QueryError ImageError) JPEG)
getBack250 = withDefaultClientEnv getBack250'

getBack250' :: ClientEnv -> Mbid -> IO (Either (QueryError ImageError) JPEG)
getBack250' = mkGetArt Internal.getBack250

getBack500 :: Mbid -> IO (Either (QueryError ImageError) JPEG)
getBack500 = withDefaultClientEnv getBack500'

getBack500' :: ClientEnv -> Mbid -> IO (Either (QueryError ImageError) JPEG)
getBack500' = mkGetArt Internal.getBack500

mkGetArt :: (a -> ClientM JPEG) -> ClientEnv -> a -> IO (Either (QueryError ImageError) JPEG)
mkGetArt f env x = fmap mapImageResult $ runClientM (f x) env

withDefaultClientEnv :: (ClientEnv -> a -> IO b) -> a -> IO b
withDefaultClientEnv f x = flip f x =<< defaultClientEnv

withDefaultClientEnv2 :: (ClientEnv -> a -> b -> IO c) -> a -> b -> IO c
withDefaultClientEnv2 f x y = do
  env <- defaultClientEnv
  f env x y

mapListingResult :: Either ServantError Listing
                 -> Either (QueryError ListingError) Listing
mapListingResult = Bifunctor.first mkListingQueryError

mapImageResult :: Either ServantError JPEG
               -> Either (QueryError ImageError) JPEG
mapImageResult = Bifunctor.first mkImageQueryError
