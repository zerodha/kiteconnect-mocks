{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderModify
    ( OrderModify (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data OrderModify = OrderModify
    { orderModifyDataOrderModify :: Maybe Data
    , statusOrderModify :: Maybe Text
    } deriving (Show)

data Data = Data
    { orderIDData :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe OrderModify
decodeTopLevel = decode

instance ToJSON OrderModify where
    toJSON (OrderModify orderModifyDataOrderModify statusOrderModify) =
        object
        [ "data" .= orderModifyDataOrderModify
        , "status" .= statusOrderModify
        ]

instance FromJSON OrderModify where
    parseJSON (Object v) = OrderModify
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
