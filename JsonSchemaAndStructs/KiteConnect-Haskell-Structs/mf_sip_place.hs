{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MfSipPlace
    ( MFSIPPlace (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFSIPPlace = MFSIPPlace
    { mfSIPPlaceDataMFSIPPlace :: Maybe Data
    , statusMFSIPPlace :: Maybe Text
    } deriving (Show)

data Data = Data
    { sipIDData :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFSIPPlace
decodeTopLevel = decode

instance ToJSON MFSIPPlace where
    toJSON (MFSIPPlace mfSIPPlaceDataMFSIPPlace statusMFSIPPlace) =
        object
        [ "data" .= mfSIPPlaceDataMFSIPPlace
        , "status" .= statusMFSIPPlace
        ]

instance FromJSON MFSIPPlace where
    parseJSON (Object v) = MFSIPPlace
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data sipIDData) =
        object
        [ "sip_id" .= sipIDData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "sip_id"
