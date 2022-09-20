{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MfOrdersInfo
    ( MFOrdersInfo (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFOrdersInfo = MFOrdersInfo
    { mfOrdersInfoDataMFOrdersInfo :: Maybe Data
    , statusMFOrdersInfo :: Maybe Text
    } deriving (Show)

data Data = Data
    { amountData :: Maybe Int
    , averagePriceData :: Maybe Int
    , exchangeOrderIDData :: Maybe (Maybe Text)
    , exchangeTimestampData :: Maybe (Maybe Text)
    , folioData :: Maybe (Maybe Text)
    , fundData :: Maybe Text
    , lastPriceData :: Maybe Float
    , lastPriceDateData :: Maybe Text
    , orderIDData :: Maybe Text
    , orderTimestampData :: Maybe Text
    , placedByData :: Maybe Text
    , purchaseTypeData :: Maybe Text
    , quantityData :: Maybe Int
    , settlementIDData :: Maybe (Maybe Text)
    , statusData :: Maybe Text
    , statusMessageData :: Maybe Text
    , tagData :: Maybe (Maybe Text)
    , tradingsymbolData :: Maybe Text
    , transactionTypeData :: Maybe Text
    , varietyData :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFOrdersInfo
decodeTopLevel = decode

instance ToJSON MFOrdersInfo where
    toJSON (MFOrdersInfo mfOrdersInfoDataMFOrdersInfo statusMFOrdersInfo) =
        object
        [ "data" .= mfOrdersInfoDataMFOrdersInfo
        , "status" .= statusMFOrdersInfo
        ]

instance FromJSON MFOrdersInfo where
    parseJSON (Object v) = MFOrdersInfo
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data amountData averagePriceData exchangeOrderIDData exchangeTimestampData folioData fundData lastPriceData lastPriceDateData orderIDData orderTimestampData placedByData purchaseTypeData quantityData settlementIDData statusData statusMessageData tagData tradingsymbolData transactionTypeData varietyData) =
        object
        [ "amount" .= amountData
        , "average_price" .= averagePriceData
        , "exchange_order_id" .= exchangeOrderIDData
        , "exchange_timestamp" .= exchangeTimestampData
        , "folio" .= folioData
        , "fund" .= fundData
        , "last_price" .= lastPriceData
        , "last_price_date" .= lastPriceDateData
        , "order_id" .= orderIDData
        , "order_timestamp" .= orderTimestampData
        , "placed_by" .= placedByData
        , "purchase_type" .= purchaseTypeData
        , "quantity" .= quantityData
        , "settlement_id" .= settlementIDData
        , "status" .= statusData
        , "status_message" .= statusMessageData
        , "tag" .= tagData
        , "tradingsymbol" .= tradingsymbolData
        , "transaction_type" .= transactionTypeData
        , "variety" .= varietyData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
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
