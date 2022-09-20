{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MfOrderCancel
    ( MFOrderCancel (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFOrderCancel = MFOrderCancel
    { mfOrderCancelDataMFOrderCancel :: Maybe Data
    , statusMFOrderCancel :: Maybe Text
    } deriving (Show)

data Data = Data
    { orderIDData :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFOrderCancel
decodeTopLevel = decode

instance ToJSON MFOrderCancel where
    toJSON (MFOrderCancel mfOrderCancelDataMFOrderCancel statusMFOrderCancel) =
        object
        [ "data" .= mfOrderCancelDataMFOrderCancel
        , "status" .= statusMFOrderCancel
        ]

instance FromJSON MFOrderCancel where
    parseJSON (Object v) = MFOrderCancel
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
