{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module TickerFull
    ( TickerFull (..)
    , TriggerRangeElement (..)
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

type TickerFull = Vector TriggerRangeElement

data TriggerRangeElement = TriggerRangeElement
    { averageTradedPriceTriggerRangeElement :: Maybe Float
    , changeTriggerRangeElement :: Maybe Float
    , depthTriggerRangeElement :: Maybe Depth
    , exchangeTimestampTriggerRangeElement :: Maybe Text
    , instrumentTokenTriggerRangeElement :: Maybe Int
    , lastPriceTriggerRangeElement :: Maybe Int
    , lastTradeTimeTriggerRangeElement :: Maybe Text
    , lastTradedQuantityTriggerRangeElement :: Maybe Int
    , modeTriggerRangeElement :: Maybe Text
    , ohlcTriggerRangeElement :: Maybe Ohlc
    , oiTriggerRangeElement :: Maybe Int
    , oiDayHighTriggerRangeElement :: Maybe Int
    , oiDayLowTriggerRangeElement :: Maybe Int
    , totalBuyQuantityTriggerRangeElement :: Maybe Int
    , totalSellQuantityTriggerRangeElement :: Maybe Int
    , tradableTriggerRangeElement :: Maybe Bool
    , volumeTradedTriggerRangeElement :: Maybe Int
    } deriving (Show)

data Depth = Depth
    { buyDepth :: Maybe (Vector Buy)
    , sellDepth :: Maybe (Vector Buy)
    } deriving (Show)

data Buy = Buy
    { ordersBuy :: Maybe Int
    , priceBuy :: Maybe Int
    , quantityBuy :: Maybe Int
    } deriving (Show)

data Ohlc = Ohlc
    { closeOhlc :: Maybe Int
    , highOhlc :: Maybe Int
    , lowOhlc :: Maybe Int
    , openOhlc :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe TickerFull
decodeTopLevel = decode

instance ToJSON TriggerRangeElement where
    toJSON (TriggerRangeElement averageTradedPriceTriggerRangeElement changeTriggerRangeElement depthTriggerRangeElement exchangeTimestampTriggerRangeElement instrumentTokenTriggerRangeElement lastPriceTriggerRangeElement lastTradeTimeTriggerRangeElement lastTradedQuantityTriggerRangeElement modeTriggerRangeElement ohlcTriggerRangeElement oiTriggerRangeElement oiDayHighTriggerRangeElement oiDayLowTriggerRangeElement totalBuyQuantityTriggerRangeElement totalSellQuantityTriggerRangeElement tradableTriggerRangeElement volumeTradedTriggerRangeElement) =
        object
        [ "average_traded_price" .= averageTradedPriceTriggerRangeElement
        , "change" .= changeTriggerRangeElement
        , "depth" .= depthTriggerRangeElement
        , "exchange_timestamp" .= exchangeTimestampTriggerRangeElement
        , "instrument_token" .= instrumentTokenTriggerRangeElement
        , "last_price" .= lastPriceTriggerRangeElement
        , "last_trade_time" .= lastTradeTimeTriggerRangeElement
        , "last_traded_quantity" .= lastTradedQuantityTriggerRangeElement
        , "mode" .= modeTriggerRangeElement
        , "ohlc" .= ohlcTriggerRangeElement
        , "oi" .= oiTriggerRangeElement
        , "oi_day_high" .= oiDayHighTriggerRangeElement
        , "oi_day_low" .= oiDayLowTriggerRangeElement
        , "total_buy_quantity" .= totalBuyQuantityTriggerRangeElement
        , "total_sell_quantity" .= totalSellQuantityTriggerRangeElement
        , "tradable" .= tradableTriggerRangeElement
        , "volume_traded" .= volumeTradedTriggerRangeElement
        ]

instance FromJSON TriggerRangeElement where
    parseJSON (Object v) = TriggerRangeElement
        <$> v .:? "average_traded_price"
        <*> v .:? "change"
        <*> v .:? "depth"
        <*> v .:? "exchange_timestamp"
        <*> v .:? "instrument_token"
        <*> v .:? "last_price"
        <*> v .:? "last_trade_time"
        <*> v .:? "last_traded_quantity"
        <*> v .:? "mode"
        <*> v .:? "ohlc"
        <*> v .:? "oi"
        <*> v .:? "oi_day_high"
        <*> v .:? "oi_day_low"
        <*> v .:? "total_buy_quantity"
        <*> v .:? "total_sell_quantity"
        <*> v .:? "tradable"
        <*> v .:? "volume_traded"

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
