{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module GttModifyOrder
    ( GttModifyOrder (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data GttModifyOrder = GttModifyOrder
    { gttModifyOrderDataGttModifyOrder :: Maybe Data
    , statusGttModifyOrder :: Maybe Text
    } deriving (Show)

data Data = Data
    { triggerIDData :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe GttModifyOrder
decodeTopLevel = decode

instance ToJSON GttModifyOrder where
    toJSON (GttModifyOrder gttModifyOrderDataGttModifyOrder statusGttModifyOrder) =
        object
        [ "data" .= gttModifyOrderDataGttModifyOrder
        , "status" .= statusGttModifyOrder
        ]

instance FromJSON GttModifyOrder where
    parseJSON (Object v) = GttModifyOrder
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
