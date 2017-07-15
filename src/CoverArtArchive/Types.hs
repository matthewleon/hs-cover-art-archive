{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module CoverArtArchive.Types
    ( Mbid(..)
    , mbidFromString
    , mbidFromText
    , Listing
    , Image
    , ThumbnailListing
    , ImageType
    , EditID
    , FileID
    , APIURI
    ) where

import Data.Aeson.Types (FromJSON, parseJSON, typeMismatch)
import Data.Text (Text)
import Data.Text.Read as T.Read
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import GHC.Generics
import Network.URI (URI, parseAbsoluteURI)
import Servant.API (ToHttpApiData)

newtype Mbid = Mbid UUID
 deriving (Show, ToHttpApiData)

mbidFromString :: String -> Maybe Mbid
mbidFromString s = Mbid <$> UUID.fromString s

mbidFromText :: Text -> Maybe Mbid
mbidFromText t = Mbid <$> UUID.fromText t

data Listing = Listing
  { release :: APIURI
  , images  :: [Image]
  } deriving (Show, Generic)

instance FromJSON Listing

data Image = Image
  { types    :: [ImageType]
  , front    :: Bool
  , back     :: Bool
  , edit     :: EditID
  , image    :: APIURI
  , comment  :: Text
  , approved :: Bool
  , id       :: FileID
  , thumbnails :: ThumbnailListing
  } deriving (Show, Generic)
instance FromJSON Image

data ThumbnailListing = ThumbnailListing
  { large :: APIURI
  , small :: APIURI
  } deriving (Show, Generic)
instance FromJSON ThumbnailListing

data ImageType =
    Front
  | Back
  | Booklet
  | Medium
  | Tray
  | Obi
  | Spine
  | Track
  | Liner
  | Sticker 
  | Poster
  | Watermark
  | Other
  deriving (Show, Generic)
instance FromJSON ImageType

newtype EditID = EditID Integer
  deriving (Show, Generic, FromJSON)

newtype FileID = FileID Integer
  deriving (Show, ToHttpApiData)
instance FromJSON FileID where
  parseJSON val = mkParser =<< T.Read.decimal <$> parseJSON val 
    where
    mkParser (Right (fid, "")) = pure $ FileID fid
    mkParser _                 = typeMismatch "fileID" val

newtype APIURI = APIURI URI
  deriving (Show)
instance FromJSON APIURI where
  parseJSON val = mkParser =<< parseAbsoluteURI <$> parseJSON val 
    where
    mkParser = maybe (typeMismatch "URI" val) (pure . APIURI)

