{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Ohlc
    ( Ohlc (..)
    , Datum (..)
    , OhlcClass (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Ohlc = Ohlc
    { ohlcDataOhlc :: Maybe (HashMap Text Datum)
    , statusOhlc :: Maybe Text
    } deriving (Show)

data Datum = Datum
    { instrumentTokenDatum :: Maybe Int
    , lastPriceDatum :: Maybe Int
    , ohlcDatum :: Maybe OhlcClass
    } deriving (Show)

data OhlcClass = OhlcClass
    { closeOhlcClass :: Maybe Float
    , highOhlcClass :: Maybe Float
    , lowOhlcClass :: Maybe Float
    , openOhlcClass :: Maybe Float
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Ohlc
decodeTopLevel = decode

instance ToJSON Ohlc where
    toJSON (Ohlc ohlcDataOhlc statusOhlc) =
        object
        [ "data" .= ohlcDataOhlc
        , "status" .= statusOhlc
        ]

instance FromJSON Ohlc where
    parseJSON (Object v) = Ohlc
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Datum where
    toJSON (Datum instrumentTokenDatum lastPriceDatum ohlcDatum) =
        object
        [ "instrument_token" .= instrumentTokenDatum
        , "last_price" .= lastPriceDatum
        , "ohlc" .= ohlcDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .:? "instrument_token"
        <*> v .:? "last_price"
        <*> v .:? "ohlc"

instance ToJSON OhlcClass where
    toJSON (OhlcClass closeOhlcClass highOhlcClass lowOhlcClass openOhlcClass) =
        object
        [ "close" .= closeOhlcClass
        , "high" .= highOhlcClass
        , "low" .= lowOhlcClass
        , "open" .= openOhlcClass
        ]

instance FromJSON OhlcClass where
    parseJSON (Object v) = OhlcClass
        <$> v .:? "close"
        <*> v .:? "high"
        <*> v .:? "low"
        <*> v .:? "open"
