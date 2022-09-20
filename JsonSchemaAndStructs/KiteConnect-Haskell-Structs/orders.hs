{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Orders
    ( Orders (..)
    , Datum (..)
    , Meta (..)
    , Iceberg (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Orders = Orders
    { ordersDataOrders :: Maybe (Vector Datum)
    , statusOrders :: Maybe Text
    } deriving (Show)

data Datum = Datum
    { averagePriceDatum :: Maybe Int
    , cancelledQuantityDatum :: Maybe Int
    , disclosedQuantityDatum :: Maybe Int
    , exchangeDatum :: Maybe Text
    , exchangeOrderIDDatum :: Maybe Text
    , exchangeTimestampDatum :: Maybe Text
    , exchangeUpdateTimestampDatum :: Maybe Text
    , filledQuantityDatum :: Maybe Int
    , guidDatum :: Maybe Text
    , instrumentTokenDatum :: Maybe Int
    , marketProtectionDatum :: Maybe Int
    , metaDatum :: Maybe Meta
    , modifiedDatum :: Maybe Bool
    , orderIDDatum :: Maybe Text
    , orderTimestampDatum :: Maybe Text
    , orderTypeDatum :: Maybe Text
    , parentOrderIDDatum :: Maybe (Maybe Text)
    , pendingQuantityDatum :: Maybe Int
    , placedByDatum :: Maybe Text
    , priceDatum :: Maybe Int
    , productDatum :: Maybe Text
    , quantityDatum :: Maybe Int
    , statusDatum :: Maybe Text
    , statusMessageDatum :: Maybe Text
    , statusMessageRawDatum :: Maybe Text
    , tagDatum :: Maybe Text
    , tagsDatum :: Maybe (Vector Text)
    , tradingsymbolDatum :: Maybe Text
    , transactionTypeDatum :: Maybe Text
    , triggerPriceDatum :: Maybe Int
    , validityDatum :: Maybe Text
    , validityTTLDatum :: Maybe Int
    , varietyDatum :: Maybe Text
    } deriving (Show)

data Meta = Meta
    { icebergMeta :: Maybe Iceberg
    } deriving (Show)

data Iceberg = Iceberg
    { legIceberg :: Maybe Int
    , legQuantityIceberg :: Maybe Int
    , legsIceberg :: Maybe Int
    , remainingQuantityIceberg :: Maybe Int
    , totalQuantityIceberg :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Orders
decodeTopLevel = decode

instance ToJSON Orders where
    toJSON (Orders ordersDataOrders statusOrders) =
        object
        [ "data" .= ordersDataOrders
        , "status" .= statusOrders
        ]

instance FromJSON Orders where
    parseJSON (Object v) = Orders
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Datum where
    toJSON (Datum averagePriceDatum cancelledQuantityDatum disclosedQuantityDatum exchangeDatum exchangeOrderIDDatum exchangeTimestampDatum exchangeUpdateTimestampDatum filledQuantityDatum guidDatum instrumentTokenDatum marketProtectionDatum metaDatum modifiedDatum orderIDDatum orderTimestampDatum orderTypeDatum parentOrderIDDatum pendingQuantityDatum placedByDatum priceDatum productDatum quantityDatum statusDatum statusMessageDatum statusMessageRawDatum tagDatum tagsDatum tradingsymbolDatum transactionTypeDatum triggerPriceDatum validityDatum validityTTLDatum varietyDatum) =
        object
        [ "average_price" .= averagePriceDatum
        , "cancelled_quantity" .= cancelledQuantityDatum
        , "disclosed_quantity" .= disclosedQuantityDatum
        , "exchange" .= exchangeDatum
        , "exchange_order_id" .= exchangeOrderIDDatum
        , "exchange_timestamp" .= exchangeTimestampDatum
        , "exchange_update_timestamp" .= exchangeUpdateTimestampDatum
        , "filled_quantity" .= filledQuantityDatum
        , "guid" .= guidDatum
        , "instrument_token" .= instrumentTokenDatum
        , "market_protection" .= marketProtectionDatum
        , "meta" .= metaDatum
        , "modified" .= modifiedDatum
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
        , "status_message_raw" .= statusMessageRawDatum
        , "tag" .= tagDatum
        , "tags" .= tagsDatum
        , "tradingsymbol" .= tradingsymbolDatum
        , "transaction_type" .= transactionTypeDatum
        , "trigger_price" .= triggerPriceDatum
        , "validity" .= validityDatum
        , "validity_ttl" .= validityTTLDatum
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
        <*> v .:? "exchange_update_timestamp"
        <*> v .:? "filled_quantity"
        <*> v .:? "guid"
        <*> v .:? "instrument_token"
        <*> v .:? "market_protection"
        <*> v .:? "meta"
        <*> v .:? "modified"
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
        <*> v .:? "tags"
        <*> v .:? "tradingsymbol"
        <*> v .:? "transaction_type"
        <*> v .:? "trigger_price"
        <*> v .:? "validity"
        <*> v .:? "validity_ttl"
        <*> v .:? "variety"

instance ToJSON Meta where
    toJSON (Meta icebergMeta) =
        object
        [ "iceberg" .= icebergMeta
        ]

instance FromJSON Meta where
    parseJSON (Object v) = Meta
        <$> v .:? "iceberg"

instance ToJSON Iceberg where
    toJSON (Iceberg legIceberg legQuantityIceberg legsIceberg remainingQuantityIceberg totalQuantityIceberg) =
        object
        [ "leg" .= legIceberg
        , "leg_quantity" .= legQuantityIceberg
        , "legs" .= legsIceberg
        , "remaining_quantity" .= remainingQuantityIceberg
        , "total_quantity" .= totalQuantityIceberg
        ]

instance FromJSON Iceberg where
    parseJSON (Object v) = Iceberg
        <$> v .:? "leg"
        <*> v .:? "leg_quantity"
        <*> v .:? "legs"
        <*> v .:? "remaining_quantity"
        <*> v .:? "total_quantity"
