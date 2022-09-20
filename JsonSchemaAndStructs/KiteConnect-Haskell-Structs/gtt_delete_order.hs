{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module GttDeleteOrder
    ( GttDeleteOrder (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data GttDeleteOrder = GttDeleteOrder
    { gttDeleteOrderDataGttDeleteOrder :: Maybe Data
    , statusGttDeleteOrder :: Maybe Text
    } deriving (Show)

data Data = Data
    { triggerIDData :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe GttDeleteOrder
decodeTopLevel = decode

instance ToJSON GttDeleteOrder where
    toJSON (GttDeleteOrder gttDeleteOrderDataGttDeleteOrder statusGttDeleteOrder) =
        object
        [ "data" .= gttDeleteOrderDataGttDeleteOrder
        , "status" .= statusGttDeleteOrder
        ]

instance FromJSON GttDeleteOrder where
    parseJSON (Object v) = GttDeleteOrder
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data triggerIDData) =
        object
        [ "trigger_id" .= triggerIDData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "trigger_id"
