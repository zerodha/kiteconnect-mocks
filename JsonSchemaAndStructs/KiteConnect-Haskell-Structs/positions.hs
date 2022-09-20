{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Positions
    ( Positions (..)
    , Data (..)
    , Day (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Positions = Positions
    { positionsDataPositions :: Maybe Data
    , statusPositions :: Maybe Text
    } deriving (Show)

data Data = Data
    { dayData :: Maybe (Vector Day)
    , netData :: Maybe (Vector Day)
    } deriving (Show)

data Day = Day
    { averagePriceDay :: Maybe Float
    , buyM2MDay :: Maybe Int
    , buyPriceDay :: Maybe Float
    , buyQuantityDay :: Maybe Int
    , buyValueDay :: Maybe Int
    , closePriceDay :: Maybe Int
    , dayBuyPriceDay :: Maybe Float
    , dayBuyQuantityDay :: Maybe Int
    , dayBuyValueDay :: Maybe Int
    , daySellPriceDay :: Maybe Int
    , daySellQuantityDay :: Maybe Int
    , daySellValueDay :: Maybe Int
    , exchangeDay :: Maybe Text
    , instrumentTokenDay :: Maybe Int
    , lastPriceDay :: Maybe Float
    , m2MDay :: Maybe Int
    , multiplierDay :: Maybe Int
    , overnightQuantityDay :: Maybe Int
    , pnlDay :: Maybe Int
    , productDay :: Maybe Text
    , quantityDay :: Maybe Int
    , realisedDay :: Maybe Int
    , sellM2MDay :: Maybe Int
    , sellPriceDay :: Maybe Int
    , sellQuantityDay :: Maybe Int
    , sellValueDay :: Maybe Int
    , tradingsymbolDay :: Maybe Text
    , unrealisedDay :: Maybe Int
    , valueDay :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Positions
decodeTopLevel = decode

instance ToJSON Positions where
    toJSON (Positions positionsDataPositions statusPositions) =
        object
        [ "data" .= positionsDataPositions
        , "status" .= statusPositions
        ]

instance FromJSON Positions where
    parseJSON (Object v) = Positions
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data dayData netData) =
        object
        [ "day" .= dayData
        , "net" .= netData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "day"
        <*> v .:? "net"

instance ToJSON Day where
    toJSON (Day averagePriceDay buyM2MDay buyPriceDay buyQuantityDay buyValueDay closePriceDay dayBuyPriceDay dayBuyQuantityDay dayBuyValueDay daySellPriceDay daySellQuantityDay daySellValueDay exchangeDay instrumentTokenDay lastPriceDay m2MDay multiplierDay overnightQuantityDay pnlDay productDay quantityDay realisedDay sellM2MDay sellPriceDay sellQuantityDay sellValueDay tradingsymbolDay unrealisedDay valueDay) =
        object
        [ "average_price" .= averagePriceDay
        , "buy_m2m" .= buyM2MDay
        , "buy_price" .= buyPriceDay
        , "buy_quantity" .= buyQuantityDay
        , "buy_value" .= buyValueDay
        , "close_price" .= closePriceDay
        , "day_buy_price" .= dayBuyPriceDay
        , "day_buy_quantity" .= dayBuyQuantityDay
        , "day_buy_value" .= dayBuyValueDay
        , "day_sell_price" .= daySellPriceDay
        , "day_sell_quantity" .= daySellQuantityDay
        , "day_sell_value" .= daySellValueDay
        , "exchange" .= exchangeDay
        , "instrument_token" .= instrumentTokenDay
        , "last_price" .= lastPriceDay
        , "m2m" .= m2MDay
        , "multiplier" .= multiplierDay
        , "overnight_quantity" .= overnightQuantityDay
        , "pnl" .= pnlDay
        , "product" .= productDay
        , "quantity" .= quantityDay
        , "realised" .= realisedDay
        , "sell_m2m" .= sellM2MDay
        , "sell_price" .= sellPriceDay
        , "sell_quantity" .= sellQuantityDay
        , "sell_value" .= sellValueDay
        , "tradingsymbol" .= tradingsymbolDay
        , "unrealised" .= unrealisedDay
        , "value" .= valueDay
        ]

instance FromJSON Day where
    parseJSON (Object v) = Day
        <$> v .:? "average_price"
        <*> v .:? "buy_m2m"
        <*> v .:? "buy_price"
        <*> v .:? "buy_quantity"
        <*> v .:? "buy_value"
        <*> v .:? "close_price"
        <*> v .:? "day_buy_price"
        <*> v .:? "day_buy_quantity"
        <*> v .:? "day_buy_value"
        <*> v .:? "day_sell_price"
        <*> v .:? "day_sell_quantity"
        <*> v .:? "day_sell_value"
        <*> v .:? "exchange"
        <*> v .:? "instrument_token"
        <*> v .:? "last_price"
        <*> v .:? "m2m"
        <*> v .:? "multiplier"
        <*> v .:? "overnight_quantity"
        <*> v .:? "pnl"
        <*> v .:? "product"
        <*> v .:? "quantity"
        <*> v .:? "realised"
        <*> v .:? "sell_m2m"
        <*> v .:? "sell_price"
        <*> v .:? "sell_quantity"
        <*> v .:? "sell_value"
        <*> v .:? "tradingsymbol"
        <*> v .:? "unrealised"
        <*> v .:? "value"
