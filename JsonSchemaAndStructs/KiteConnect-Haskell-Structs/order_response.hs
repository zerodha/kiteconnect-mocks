{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderResponse
    ( OrderResponse (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data OrderResponse = OrderResponse
    { orderResponseDataOrderResponse :: Maybe Data
    , statusOrderResponse :: Maybe Text
    } deriving (Show)

data Data = Data
    { orderIDData :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe OrderResponse
decodeTopLevel = decode

instance ToJSON OrderResponse where
    toJSON (OrderResponse orderResponseDataOrderResponse statusOrderResponse) =
        object
        [ "data" .= orderResponseDataOrderResponse
        , "status" .= statusOrderResponse
        ]

instance FromJSON OrderResponse where
    parseJSON (Object v) = OrderResponse
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
