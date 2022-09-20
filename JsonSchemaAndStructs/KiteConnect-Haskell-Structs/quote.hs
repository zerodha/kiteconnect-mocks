{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Quote
    ( Quote (..)
    , Datum (..)
    , Depth (..)
    , Buy (..)
    , Ohlc (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Quote = Quote
    { quoteDataQuote :: Maybe (HashMap Text Datum)
    , statusQuote :: Maybe Text
    } deriving (Show)

data Datum = Datum
    { averagePriceDatum :: Maybe Float
    , buyQuantityDatum :: Maybe Int
    , depthDatum :: Maybe Depth
    , instrumentTokenDatum :: Maybe Int
    , lastPriceDatum :: Maybe Float
    , lastQuantityDatum :: Maybe Int
    , lastTradeTimeDatum :: Maybe Text
    , lowerCircuitLimitDatum :: Maybe Float
    , netChangeDatum :: Maybe Int
    , ohlcDatum :: Maybe Ohlc
    , oiDatum :: Maybe Int
    , oiDayHighDatum :: Maybe Int
    , oiDayLowDatum :: Maybe Int
    , sellQuantityDatum :: Maybe Int
    , timestampDatum :: Maybe Text
    , upperCircuitLimitDatum :: Maybe Float
    , volumeDatum :: Maybe Int
    } deriving (Show)

data Depth = Depth
    { buyDepth :: Maybe (Vector Buy)
    , sellDepth :: Maybe (Vector Buy)
    } deriving (Show)

data Buy = Buy
    { ordersBuy :: Maybe Int
    , priceBuy :: Maybe Float
    , quantityBuy :: Maybe Int
    } deriving (Show)

data Ohlc = Ohlc
    { closeOhlc :: Maybe Float
    , highOhlc :: Maybe Float
    , lowOhlc :: Maybe Float
    , openOhlc :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Quote
decodeTopLevel = decode

instance ToJSON Quote where
    toJSON (Quote quoteDataQuote statusQuote) =
        object
        [ "data" .= quoteDataQuote
        , "status" .= statusQuote
        ]

instance FromJSON Quote where
    parseJSON (Object v) = Quote
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Datum where
    toJSON (Datum averagePriceDatum buyQuantityDatum depthDatum instrumentTokenDatum lastPriceDatum lastQuantityDatum lastTradeTimeDatum lowerCircuitLimitDatum netChangeDatum ohlcDatum oiDatum oiDayHighDatum oiDayLowDatum sellQuantityDatum timestampDatum upperCircuitLimitDatum volumeDatum) =
        object
        [ "average_price" .= averagePriceDatum
        , "buy_quantity" .= buyQuantityDatum
        , "depth" .= depthDatum
        , "instrument_token" .= instrumentTokenDatum
        , "last_price" .= lastPriceDatum
        , "last_quantity" .= lastQuantityDatum
        , "last_trade_time" .= lastTradeTimeDatum
        , "lower_circuit_limit" .= lowerCircuitLimitDatum
        , "net_change" .= netChangeDatum
        , "ohlc" .= ohlcDatum
        , "oi" .= oiDatum
        , "oi_day_high" .= oiDayHighDatum
        , "oi_day_low" .= oiDayLowDatum
        , "sell_quantity" .= sellQuantityDatum
        , "timestamp" .= timestampDatum
        , "upper_circuit_limit" .= upperCircuitLimitDatum
        , "volume" .= volumeDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .:? "average_price"
        <*> v .:? "buy_quantity"
        <*> v .:? "depth"
        <*> v .:? "instrument_token"
        <*> v .:? "last_price"
        <*> v .:? "last_quantity"
        <*> v .:? "last_trade_time"
        <*> v .:? "lower_circuit_limit"
        <*> v .:? "net_change"
        <*> v .:? "ohlc"
        <*> v .:? "oi"
        <*> v .:? "oi_day_high"
        <*> v .:? "oi_day_low"
        <*> v .:? "sell_quantity"
        <*> v .:? "timestamp"
        <*> v .:? "upper_circuit_limit"
        <*> v .:? "volume"

instance ToJSON Depth where
    toJSON (Depth buyDepth sellDepth) =
        object
        [ "buy" .= buyDepth
        , "sell" .= sellDepth
        ]

instance FromJSON Depth where
    parseJSON (Object v) = Depth
        <$> v .:? "buy"
        <*> v .:? "sell"

instance ToJSON Buy where
    toJSON (Buy ordersBuy priceBuy quantityBuy) =
        object
        [ "orders" .= ordersBuy
        , "price" .= priceBuy
        , "quantity" .= quantityBuy
        ]

instance FromJSON Buy where
    parseJSON (Object v) = Buy
        <$> v .:? "orders"
        <*> v .:? "price"
        <*> v .:? "quantity"

instance ToJSON Ohlc where
    toJSON (Ohlc closeOhlc highOhlc lowOhlc openOhlc) =
        object
        [ "close" .= closeOhlc
        , "high" .= highOhlc
        , "low" .= lowOhlc
        , "open" .= openOhlc
        ]

instance FromJSON Ohlc where
    parseJSON (Object v) = Ohlc
        <$> v .:? "close"
        <*> v .:? "high"
        <*> v .:? "low"
        <*> v .:? "open"
