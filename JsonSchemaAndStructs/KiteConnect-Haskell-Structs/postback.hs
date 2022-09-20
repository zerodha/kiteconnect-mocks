{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Postback
    ( Postback (..)
    , Meta (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Postback = Postback
    { appIDPostback :: Maybe Int
    , averagePricePostback :: Maybe Int
    , cancelledQuantityPostback :: Maybe Int
    , checksumPostback :: Maybe Text
    , disclosedQuantityPostback :: Maybe Int
    , exchangePostback :: Maybe Text
    , exchangeOrderIDPostback :: Maybe Text
    , exchangeTimestampPostback :: Maybe Text
    , exchangeUpdateTimestampPostback :: Maybe Text
    , filledQuantityPostback :: Maybe Int
    , guidPostback :: Maybe Text
    , instrumentTokenPostback :: Maybe Int
    , marketProtectionPostback :: Maybe Int
    , metaPostback :: Maybe Meta
    , orderIDPostback :: Maybe Text
    , orderTimestampPostback :: Maybe Text
    , orderTypePostback :: Maybe Text
    , parentOrderIDPostback :: Maybe (Maybe Text)
    , pendingQuantityPostback :: Maybe Int
    , placedByPostback :: Maybe Text
    , pricePostback :: Maybe Int
    , productPostback :: Maybe Text
    , quantityPostback :: Maybe Int
    , statusPostback :: Maybe Text
    , statusMessagePostback :: Maybe (Maybe Text)
    , statusMessageRawPostback :: Maybe (Maybe Text)
    , tagPostback :: Maybe (Maybe Text)
    , tradingsymbolPostback :: Maybe Text
    , transactionTypePostback :: Maybe Text
    , triggerPricePostback :: Maybe Int
    , unfilledQuantityPostback :: Maybe Int
    , userIDPostback :: Maybe Text
    , validityPostback :: Maybe Text
    , varietyPostback :: Maybe Text
    } deriving (Show)

data Meta = Meta
    {
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Postback
decodeTopLevel = decode

instance ToJSON Postback where
    toJSON (Postback appIDPostback averagePricePostback cancelledQuantityPostback checksumPostback disclosedQuantityPostback exchangePostback exchangeOrderIDPostback exchangeTimestampPostback exchangeUpdateTimestampPostback filledQuantityPostback guidPostback instrumentTokenPostback marketProtectionPostback metaPostback orderIDPostback orderTimestampPostback orderTypePostback parentOrderIDPostback pendingQuantityPostback placedByPostback pricePostback productPostback quantityPostback statusPostback statusMessagePostback statusMessageRawPostback tagPostback tradingsymbolPostback transactionTypePostback triggerPricePostback unfilledQuantityPostback userIDPostback validityPostback varietyPostback) =
        object
        [ "app_id" .= appIDPostback
        , "average_price" .= averagePricePostback
        , "cancelled_quantity" .= cancelledQuantityPostback
        , "checksum" .= checksumPostback
        , "disclosed_quantity" .= disclosedQuantityPostback
        , "exchange" .= exchangePostback
        , "exchange_order_id" .= exchangeOrderIDPostback
        , "exchange_timestamp" .= exchangeTimestampPostback
        , "exchange_update_timestamp" .= exchangeUpdateTimestampPostback
        , "filled_quantity" .= filledQuantityPostback
        , "guid" .= guidPostback
        , "instrument_token" .= instrumentTokenPostback
        , "market_protection" .= marketProtectionPostback
        , "meta" .= metaPostback
        , "order_id" .= orderIDPostback
        , "order_timestamp" .= orderTimestampPostback
        , "order_type" .= orderTypePostback
        , "parent_order_id" .= parentOrderIDPostback
        , "pending_quantity" .= pendingQuantityPostback
        , "placed_by" .= placedByPostback
        , "price" .= pricePostback
        , "product" .= productPostback
        , "quantity" .= quantityPostback
        , "status" .= statusPostback
        , "status_message" .= statusMessagePostback
        , "status_message_raw" .= statusMessageRawPostback
        , "tag" .= tagPostback
        , "tradingsymbol" .= tradingsymbolPostback
        , "transaction_type" .= transactionTypePostback
        , "trigger_price" .= triggerPricePostback
        , "unfilled_quantity" .= unfilledQuantityPostback
        , "user_id" .= userIDPostback
        , "validity" .= validityPostback
        , "variety" .= varietyPostback
        ]

instance FromJSON Postback where
    parseJSON (Object v) = Postback
        <$> v .:? "app_id"
        <*> v .:? "average_price"
        <*> v .:? "cancelled_quantity"
        <*> v .:? "checksum"
        <*> v .:? "disclosed_quantity"
        <*> v .:? "exchange"
        <*> v .:? "exchange_order_id"
        <*> v .:? "exchange_timestamp"
        <*> v .:? "exchange_update_timestamp"
        <*> v .:? "filled_quantity"
        <*> v .:? "guid"
        <*> v .:? "instrument_token"
        <*> v .:? "market_protection"
        <*> v .:? "meta"
        <*> v .:? "order_id"
        <*> v .:? "order_timestamp"
        <*> v .:? "order_type"
        <*> v .:? "parent_order_id"
        <*> v .:? "pending_quantity"
        <*> v .:? "placed_by"
        <*> v .:? "price"
        <*> v .:? "product"
        <*> v .:? "quantity"
        <*> v .:? "status"
        <*> v .:? "status_message"
        <*> v .:? "status_message_raw"
        <*> v .:? "tag"
        <*> v .:? "tradingsymbol"
        <*> v .:? "transaction_type"
        <*> v .:? "trigger_price"
        <*> v .:? "unfilled_quantity"
        <*> v .:? "user_id"
        <*> v .:? "validity"
        <*> v .:? "variety"

instance ToJSON Meta where
    toJSON = \_ -> emptyObject

instance FromJSON Meta where
    parseJSON emptyObject = return Meta
