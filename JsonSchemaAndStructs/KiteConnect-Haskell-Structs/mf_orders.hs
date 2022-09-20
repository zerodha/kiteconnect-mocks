{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MfOrders
    ( MFOrders (..)
    , Datum (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFOrders = MFOrders
    { mfOrdersDataMFOrders :: Maybe (Vector Datum)
    , statusMFOrders :: Maybe Text
    } deriving (Show)

data Datum = Datum
    { amountDatum :: Maybe Int
    , averagePriceDatum :: Maybe Int
    , exchangeOrderIDDatum :: Maybe Text
    , exchangeTimestampDatum :: Maybe Text
    , folioDatum :: Maybe (Maybe Text)
    , fundDatum :: Maybe Text
    , lastPriceDatum :: Maybe Float
    , lastPriceDateDatum :: Maybe Text
    , orderIDDatum :: Maybe Text
    , orderTimestampDatum :: Maybe Text
    , placedByDatum :: Maybe Text
    , purchaseTypeDatum :: Maybe Text
    , quantityDatum :: Maybe Int
    , settlementIDDatum :: Maybe Text
    , statusDatum :: Maybe Text
    , statusMessageDatum :: Maybe Text
    , tagDatum :: Maybe Text
    , tradingsymbolDatum :: Maybe Text
    , transactionTypeDatum :: Maybe Text
    , varietyDatum :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFOrders
decodeTopLevel = decode

instance ToJSON MFOrders where
    toJSON (MFOrders mfOrdersDataMFOrders statusMFOrders) =
        object
        [ "data" .= mfOrdersDataMFOrders
        , "status" .= statusMFOrders
        ]

instance FromJSON MFOrders where
    parseJSON (Object v) = MFOrders
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Datum where
    toJSON (Datum amountDatum averagePriceDatum exchangeOrderIDDatum exchangeTimestampDatum folioDatum fundDatum lastPriceDatum lastPriceDateDatum orderIDDatum orderTimestampDatum placedByDatum purchaseTypeDatum quantityDatum settlementIDDatum statusDatum statusMessageDatum tagDatum tradingsymbolDatum transactionTypeDatum varietyDatum) =
        object
        [ "amount" .= amountDatum
        , "average_price" .= averagePriceDatum
        , "exchange_order_id" .= exchangeOrderIDDatum
        , "exchange_timestamp" .= exchangeTimestampDatum
        , "folio" .= folioDatum
        , "fund" .= fundDatum
        , "last_price" .= lastPriceDatum
        , "last_price_date" .= lastPriceDateDatum
        , "order_id" .= orderIDDatum
        , "order_timestamp" .= orderTimestampDatum
        , "placed_by" .= placedByDatum
        , "purchase_type" .= purchaseTypeDatum
        , "quantity" .= quantityDatum
        , "settlement_id" .= settlementIDDatum
        , "status" .= statusDatum
        , "status_message" .= statusMessageDatum
        , "tag" .= tagDatum
        , "tradingsymbol" .= tradingsymbolDatum
        , "transaction_type" .= transactionTypeDatum
        , "variety" .= varietyDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .:? "amount"
        <*> v .:? "average_price"
        <*> v .:? "exchange_order_id"
        <*> v .:? "exchange_timestamp"
        <*> v .:? "folio"
        <*> v .:? "fund"
        <*> v .:? "last_price"
        <*> v .:? "last_price_date"
        <*> v .:? "order_id"
        <*> v .:? "order_timestamp"
        <*> v .:? "placed_by"
        <*> v .:? "purchase_type"
        <*> v .:? "quantity"
        <*> v .:? "settlement_id"
        <*> v .:? "status"
        <*> v .:? "status_message"
        <*> v .:? "tag"
        <*> v .:? "tradingsymbol"
        <*> v .:? "transaction_type"
        <*> v .:? "variety"
