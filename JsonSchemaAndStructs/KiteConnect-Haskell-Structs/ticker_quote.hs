{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module TickerQuote
    ( TickerQuote (..)
    , TriggerRangeElement (..)
    , Ohlc (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

type TickerQuote = Vector TriggerRangeElement

data TriggerRangeElement = TriggerRangeElement
    { averageTradedPriceTriggerRangeElement :: Maybe Float
    , changeTriggerRangeElement :: Maybe Float
    , instrumentTokenTriggerRangeElement :: Maybe Int
    , lastPriceTriggerRangeElement :: Maybe Int
    , lastTradedQuantityTriggerRangeElement :: Maybe Int
    , modeTriggerRangeElement :: Maybe Text
    , ohlcTriggerRangeElement :: Maybe Ohlc
    , totalBuyQuantityTriggerRangeElement :: Maybe Int
    , totalSellQuantityTriggerRangeElement :: Maybe Int
    , tradableTriggerRangeElement :: Maybe Bool
    , volumeTradedTriggerRangeElement :: Maybe Int
    } deriving (Show)

data Ohlc = Ohlc
    { closeOhlc :: Maybe Int
    , highOhlc :: Maybe Int
    , lowOhlc :: Maybe Int
    , openOhlc :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe TickerQuote
decodeTopLevel = decode

instance ToJSON TriggerRangeElement where
    toJSON (TriggerRangeElement averageTradedPriceTriggerRangeElement changeTriggerRangeElement instrumentTokenTriggerRangeElement lastPriceTriggerRangeElement lastTradedQuantityTriggerRangeElement modeTriggerRangeElement ohlcTriggerRangeElement totalBuyQuantityTriggerRangeElement totalSellQuantityTriggerRangeElement tradableTriggerRangeElement volumeTradedTriggerRangeElement) =
        object
        [ "average_traded_price" .= averageTradedPriceTriggerRangeElement
        , "change" .= changeTriggerRangeElement
        , "instrument_token" .= instrumentTokenTriggerRangeElement
        , "last_price" .= lastPriceTriggerRangeElement
        , "last_traded_quantity" .= lastTradedQuantityTriggerRangeElement
        , "mode" .= modeTriggerRangeElement
        , "ohlc" .= ohlcTriggerRangeElement
        , "total_buy_quantity" .= totalBuyQuantityTriggerRangeElement
        , "total_sell_quantity" .= totalSellQuantityTriggerRangeElement
        , "tradable" .= tradableTriggerRangeElement
        , "volume_traded" .= volumeTradedTriggerRangeElement
        ]

instance FromJSON TriggerRangeElement where
    parseJSON (Object v) = TriggerRangeElement
        <$> v .:? "average_traded_price"
        <*> v .:? "change"
        <*> v .:? "instrument_token"
        <*> v .:? "last_price"
        <*> v .:? "last_traded_quantity"
        <*> v .:? "mode"
        <*> v .:? "ohlc"
        <*> v .:? "total_buy_quantity"
        <*> v .:? "total_sell_quantity"
        <*> v .:? "tradable"
        <*> v .:? "volume_traded"

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
