{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Ltp
    ( Ltp (..)
    , Datum (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Ltp = Ltp
    { ltpDataLtp :: Maybe (HashMap Text Datum)
    , statusLtp :: Maybe Text
    } deriving (Show)

data Datum = Datum
    { instrumentTokenDatum :: Maybe Int
    , lastPriceDatum :: Maybe Float
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Ltp
decodeTopLevel = decode

instance ToJSON Ltp where
    toJSON (Ltp ltpDataLtp statusLtp) =
        object
        [ "data" .= ltpDataLtp
        , "status" .= statusLtp
        ]

instance FromJSON Ltp where
    parseJSON (Object v) = Ltp
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Datum where
    toJSON (Datum instrumentTokenDatum lastPriceDatum) =
        object
        [ "instrument_token" .= instrumentTokenDatum
        , "last_price" .= lastPriceDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .:? "instrument_token"
        <*> v .:? "last_price"
