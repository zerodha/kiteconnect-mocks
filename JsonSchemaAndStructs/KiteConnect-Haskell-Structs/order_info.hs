{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderInfo
    ( OrderInfo (..)
    , Datum (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data OrderInfo = OrderInfo
    { orderInfoDataOrderInfo :: Maybe (Vector Datum)
    , statusOrderInfo :: Maybe Text
    } deriving (Show)

data Datum = Datum
    { averagePriceDatum :: Maybe Int
    , cancelledQuantityDatum :: Maybe Int
    , disclosedQuantityDatum :: Maybe Int
    , exchangeDatum :: Maybe Text
    , exchangeOrderIDDatum :: Maybe Text
    , exchangeTimestampDatum :: Maybe Text
    , filledQuantityDatum :: Maybe Int
    , instrumentTokenDatum :: Maybe Int
    , orderIDDatum :: Maybe Text
    , orderTimestampDatum :: Maybe Text
    , orderTypeDatum :: Maybe Text
    , parentOrderIDDatum :: Maybe (Maybe Text)
    , pendingQuantityDatum :: Maybe Int
    , placedByDatum :: Maybe Text
    , priceDatum :: Maybe Float
    , productDatum :: Maybe Text
    , quantityDatum :: Maybe Int
    , statusDatum :: Maybe Text
    , statusMessageDatum :: Maybe (Maybe Text)
    , tagDatum :: Maybe (Maybe Text)
    , tradingsymbolDatum :: Maybe Text
    , transactionTypeDatum :: Maybe Text
    , triggerPriceDatum :: Maybe Int
    , validityDatum :: Maybe Text
    , varietyDatum :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe OrderInfo
decodeTopLevel = decode

instance ToJSON OrderInfo where
    toJSON (OrderInfo orderInfoDataOrderInfo statusOrderInfo) =
        object
        [ "data" .= orderInfoDataOrderInfo
        , "status" .= statusOrderInfo
        ]

instance FromJSON OrderInfo where
    parseJSON (Object v) = OrderInfo
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Datum where
    toJSON (Datum averagePriceDatum cancelledQuantityDatum disclosedQuantityDatum exchangeDatum exchangeOrderIDDatum exchangeTimestampDatum filledQuantityDatum instrumentTokenDatum orderIDDatum orderTimestampDatum orderTypeDatum parentOrderIDDatum pendingQuantityDatum placedByDatum priceDatum productDatum quantityDatum statusDatum statusMessageDatum tagDatum tradingsymbolDatum transactionTypeDatum triggerPriceDatum validityDatum varietyDatum) =
        object
        [ "average_price" .= averagePriceDatum
        , "cancelled_quantity" .= cancelledQuantityDatum
        , "disclosed_quantity" .= disclosedQuantityDatum
        , "exchange" .= exchangeDatum
        , "exchange_order_id" .= exchangeOrderIDDatum
        , "exchange_timestamp" .= exchangeTimestampDatum
        , "filled_quantity" .= filledQuantityDatum
        , "instrument_token" .= instrumentTokenDatum
        , "order_id" .= orderIDDatum
        , "order_timestamp" .= orderTimestampDatum
        , "order_type" .= orderTypeDatum
        , "parent_order_id" .= parentOrderIDDatum
        , "pending_quantity" .= pendingQuantityDatum
        , "placed_by" .= placedByDatum
        , "price" .= priceDatum
        , "product" .= productDatum
        , "quantity" .= quantityDatum
        , "status" .= statusDatum
        , "status_message" .= statusMessageDatum
        , "tag" .= tagDatum
        , "tradingsymbol" .= tradingsymbolDatum
        , "transaction_type" .= transactionTypeDatum
        , "trigger_price" .= triggerPriceDatum
        , "validity" .= validityDatum
        , "variety" .= varietyDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .:? "average_price"
        <*> v .:? "cancelled_quantity"
        <*> v .:? "disclosed_quantity"
        <*> v .:? "exchange"
        <*> v .:? "exchange_order_id"
        <*> v .:? "exchange_timestamp"
        <*> v .:? "filled_quantity"
        <*> v .:? "instrument_token"
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
        <*> v .:? "tag"
        <*> v .:? "tradingsymbol"
        <*> v .:? "transaction_type"
        <*> v .:? "trigger_price"
        <*> v .:? "validity"
        <*> v .:? "variety"
