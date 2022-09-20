{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderCancel
    ( OrderCancel (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data OrderCancel = OrderCancel
    { orderCancelDataOrderCancel :: Maybe Data
    , statusOrderCancel :: Maybe Text
    } deriving (Show)

data Data = Data
    { orderIDData :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe OrderCancel
decodeTopLevel = decode

instance ToJSON OrderCancel where
    toJSON (OrderCancel orderCancelDataOrderCancel statusOrderCancel) =
        object
        [ "data" .= orderCancelDataOrderCancel
        , "status" .= statusOrderCancel
        ]

instance FromJSON OrderCancel where
    parseJSON (Object v) = OrderCancel
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
