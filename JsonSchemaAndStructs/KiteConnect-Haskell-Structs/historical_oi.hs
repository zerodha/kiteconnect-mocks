{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module HistoricalOi
    ( HistoricalOi (..)
    , Data (..)
    , Candle (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data HistoricalOi = HistoricalOi
    { historicalOiDataHistoricalOi :: Maybe Data
    , statusHistoricalOi :: Maybe Text
    } deriving (Show)

data Data = Data
    { candlesData :: Maybe (Vector (Vector Candle))
    } deriving (Show)

data Candle
    = DoubleInCandle Float
    | StringInCandle Text
    deriving (Show)

decodeTopLevel :: ByteString -> Maybe HistoricalOi
decodeTopLevel = decode

instance ToJSON HistoricalOi where
    toJSON (HistoricalOi historicalOiDataHistoricalOi statusHistoricalOi) =
        object
        [ "data" .= historicalOiDataHistoricalOi
        , "status" .= statusHistoricalOi
        ]

instance FromJSON HistoricalOi where
    parseJSON (Object v) = HistoricalOi
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data candlesData) =
        object
        [ "candles" .= candlesData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "candles"

instance ToJSON Candle where
    toJSON (DoubleInCandle x) = toJSON x
    toJSON (StringInCandle x) = toJSON x

instance FromJSON Candle where
    parseJSON xs@(Number _) = (fmap DoubleInCandle . parseJSON) xs
    parseJSON xs@(String _) = (fmap StringInCandle . parseJSON) xs
