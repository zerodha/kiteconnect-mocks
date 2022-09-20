{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTrades
    ( OrderTrades (..)
    , Datum (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data OrderTrades = OrderTrades
    { orderTradesDataOrderTrades :: Maybe (Vector Datum)
    , statusOrderTrades :: Maybe Text
    } deriving (Show)

data Datum = Datum
    { averagePriceDatum :: Maybe Int
    , exchangeDatum :: Maybe Text
    , exchangeOrderIDDatum :: Maybe Text
    , exchangeTimestampDatum :: Maybe Text
    , fillTimestampDatum :: Maybe Text
    , instrumentTokenDatum :: Maybe Int
    , orderIDDatum :: Maybe Text
    , orderTimestampDatum :: Maybe Text
    , productDatum :: Maybe Text
    , quantityDatum :: Maybe Int
    , tradeIDDatum :: Maybe Text
    , tradingsymbolDatum :: Maybe Text
    , transactionTypeDatum :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe OrderTrades
decodeTopLevel = decode

instance ToJSON OrderTrades where
    toJSON (OrderTrades orderTradesDataOrderTrades statusOrderTrades) =
        object
        [ "data" .= orderTradesDataOrderTrades
        , "status" .= statusOrderTrades
        ]

instance FromJSON OrderTrades where
    parseJSON (Object v) = OrderTrades
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Datum where
    toJSON (Datum averagePriceDatum exchangeDatum exchangeOrderIDDatum exchangeTimestampDatum fillTimestampDatum instrumentTokenDatum orderIDDatum orderTimestampDatum productDatum quantityDatum tradeIDDatum tradingsymbolDatum transactionTypeDatum) =
        object
        [ "average_price" .= averagePriceDatum
        , "exchange" .= exchangeDatum
        , "exchange_order_id" .= exchangeOrderIDDatum
        , "exchange_timestamp" .= exchangeTimestampDatum
        , "fill_timestamp" .= fillTimestampDatum
        , "instrument_token" .= instrumentTokenDatum
        , "order_id" .= orderIDDatum
        , "order_timestamp" .= orderTimestampDatum
        , "product" .= productDatum
        , "quantity" .= quantityDatum
        , "trade_id" .= tradeIDDatum
        , "tradingsymbol" .= tradingsymbolDatum
        , "transaction_type" .= transactionTypeDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .:? "average_price"
        <*> v .:? "exchange"
        <*> v .:? "exchange_order_id"
        <*> v .:? "exchange_timestamp"
        <*> v .:? "fill_timestamp"
        <*> v .:? "instrument_token"
        <*> v .:? "order_id"
        <*> v .:? "order_timestamp"
        <*> v .:? "product"
        <*> v .:? "quantity"
        <*> v .:? "trade_id"
        <*> v .:? "tradingsymbol"
        <*> v .:? "transaction_type"
