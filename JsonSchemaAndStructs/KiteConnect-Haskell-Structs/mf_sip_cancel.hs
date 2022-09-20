{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MfSipCancel
    ( MFSIPCancel (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFSIPCancel = MFSIPCancel
    { mfSIPCancelDataMFSIPCancel :: Maybe Data
    , statusMFSIPCancel :: Maybe Text
    } deriving (Show)

data Data = Data
    { sipIDData :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFSIPCancel
decodeTopLevel = decode

instance ToJSON MFSIPCancel where
    toJSON (MFSIPCancel mfSIPCancelDataMFSIPCancel statusMFSIPCancel) =
        object
        [ "data" .= mfSIPCancelDataMFSIPCancel
        , "status" .= statusMFSIPCancel
        ]

instance FromJSON MFSIPCancel where
    parseJSON (Object v) = MFSIPCancel
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
