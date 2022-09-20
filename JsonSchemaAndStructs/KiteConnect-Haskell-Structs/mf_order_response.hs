{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MfOrderResponse
    ( MFOrderResponse (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFOrderResponse = MFOrderResponse
    { mfOrderResponseDataMFOrderResponse :: Maybe Data
    , statusMFOrderResponse :: Maybe Text
    } deriving (Show)

data Data = Data
    { orderIDData :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFOrderResponse
decodeTopLevel = decode

instance ToJSON MFOrderResponse where
    toJSON (MFOrderResponse mfOrderResponseDataMFOrderResponse statusMFOrderResponse) =
        object
        [ "data" .= mfOrderResponseDataMFOrderResponse
        , "status" .= statusMFOrderResponse
        ]

instance FromJSON MFOrderResponse where
    parseJSON (Object v) = MFOrderResponse
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data orderIDData) =
        object
        [ "order_id" .= orderIDData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "order_id"
