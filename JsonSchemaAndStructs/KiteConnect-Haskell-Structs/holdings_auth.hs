{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module HoldingsAuth
    ( HoldingsAuth (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data HoldingsAuth = HoldingsAuth
    { holdingsAuthDataHoldingsAuth :: Maybe Data
    , statusHoldingsAuth :: Maybe Text
    } deriving (Show)

data Data = Data
    { requestIDData :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe HoldingsAuth
decodeTopLevel = decode

instance ToJSON HoldingsAuth where
    toJSON (HoldingsAuth holdingsAuthDataHoldingsAuth statusHoldingsAuth) =
        object
        [ "data" .= holdingsAuthDataHoldingsAuth
        , "status" .= statusHoldingsAuth
        ]

instance FromJSON HoldingsAuth where
    parseJSON (Object v) = HoldingsAuth
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data requestIDData) =
        object
        [ "request_id" .= requestIDData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "request_id"
