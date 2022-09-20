{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module GttPlaceOrder
    ( GttPlaceOrder (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data GttPlaceOrder = GttPlaceOrder
    { gttPlaceOrderDataGttPlaceOrder :: Maybe Data
    , statusGttPlaceOrder :: Maybe Text
    } deriving (Show)

data Data = Data
    { triggerIDData :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe GttPlaceOrder
decodeTopLevel = decode

instance ToJSON GttPlaceOrder where
    toJSON (GttPlaceOrder gttPlaceOrderDataGttPlaceOrder statusGttPlaceOrder) =
        object
        [ "data" .= gttPlaceOrderDataGttPlaceOrder
        , "status" .= statusGttPlaceOrder
        ]

instance FromJSON GttPlaceOrder where
    parseJSON (Object v) = GttPlaceOrder
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
