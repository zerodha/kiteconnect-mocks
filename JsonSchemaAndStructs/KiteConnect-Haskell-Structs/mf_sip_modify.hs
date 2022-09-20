{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MfSipModify
    ( MFSIPModify (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFSIPModify = MFSIPModify
    { mfSIPModifyDataMFSIPModify :: Maybe Data
    , statusMFSIPModify :: Maybe Text
    } deriving (Show)

data Data = Data
    { sipIDData :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFSIPModify
decodeTopLevel = decode

instance ToJSON MFSIPModify where
    toJSON (MFSIPModify mfSIPModifyDataMFSIPModify statusMFSIPModify) =
        object
        [ "data" .= mfSIPModifyDataMFSIPModify
        , "status" .= statusMFSIPModify
        ]

instance FromJSON MFSIPModify where
    parseJSON (Object v) = MFSIPModify
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
