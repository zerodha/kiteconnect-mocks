{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MfSipInfo
    ( MFSIPInfo (..)
    , Data (..)
    , StepUp (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFSIPInfo = MFSIPInfo
    { mfSIPInfoDataMFSIPInfo :: Maybe Data
    , statusMFSIPInfo :: Maybe Text
    } deriving (Show)

data Data = Data
    { completedInstalmentsData :: Maybe Int
    , createdData :: Maybe Text
    , dividendTypeData :: Maybe Text
    , frequencyData :: Maybe Text
    , fundData :: Maybe Text
    , fundSourceData :: Maybe Text
    , instalmentAmountData :: Maybe Int
    , instalmentDayData :: Maybe Int
    , instalmentsData :: Maybe Int
    , lastInstalmentData :: Maybe Text
    , nextInstalmentData :: Maybe Text
    , pendingInstalmentsData :: Maybe Int
    , sipIDData :: Maybe Text
    , sipRegNumData :: Maybe (Maybe Text)
    , sipTypeData :: Maybe Text
    , statusData :: Maybe Text
    , stepUpData :: Maybe StepUp
    , tagData :: Maybe Text
    , tradingsymbolData :: Maybe Text
    , transactionTypeData :: Maybe Text
    , triggerPriceData :: Maybe Int
    } deriving (Show)

data StepUp = StepUp
    { the1502StepUp :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFSIPInfo
decodeTopLevel = decode

instance ToJSON MFSIPInfo where
    toJSON (MFSIPInfo mfSIPInfoDataMFSIPInfo statusMFSIPInfo) =
        object
        [ "data" .= mfSIPInfoDataMFSIPInfo
        , "status" .= statusMFSIPInfo
        ]

instance FromJSON MFSIPInfo where
    parseJSON (Object v) = MFSIPInfo
        <$> v .:? "data"
        <*> v .:? "status"

instance ToJSON Data where
    toJSON (Data completedInstalmentsData createdData dividendTypeData frequencyData fundData fundSourceData instalmentAmountData instalmentDayData instalmentsData lastInstalmentData nextInstalmentData pendingInstalmentsData sipIDData sipRegNumData sipTypeData statusData stepUpData tagData tradingsymbolData transactionTypeData triggerPriceData) =
        object
        [ "completed_instalments" .= completedInstalmentsData
        , "created" .= createdData
        , "dividend_type" .= dividendTypeData
        , "frequency" .= frequencyData
        , "fund" .= fundData
        , "fund_source" .= fundSourceData
        , "instalment_amount" .= instalmentAmountData
        , "instalment_day" .= instalmentDayData
        , "instalments" .= instalmentsData
        , "last_instalment" .= lastInstalmentData
        , "next_instalment" .= nextInstalmentData
        , "pending_instalments" .= pendingInstalmentsData
        , "sip_id" .= sipIDData
        , "sip_reg_num" .= sipRegNumData
        , "sip_type" .= sipTypeData
        , "status" .= statusData
        , "step_up" .= stepUpData
        , "tag" .= tagData
        , "tradingsymbol" .= tradingsymbolData
        , "transaction_type" .= transactionTypeData
        , "trigger_price" .= triggerPriceData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "completed_instalments"
        <*> v .:? "created"
        <*> v .:? "dividend_type"
        <*> v .:? "frequency"
        <*> v .:? "fund"
        <*> v .:? "fund_source"
        <*> v .:? "instalment_amount"
        <*> v .:? "instalment_day"
        <*> v .:? "instalments"
        <*> v .:? "last_instalment"
        <*> v .:? "next_instalment"
        <*> v .:? "pending_instalments"
        <*> v .:? "sip_id"
        <*> v .:? "sip_reg_num"
        <*> v .:? "sip_type"
        <*> v .:? "status"
        <*> v .:? "step_up"
        <*> v .:? "tag"
        <*> v .:? "tradingsymbol"
        <*> v .:? "transaction_type"
        <*> v .:? "trigger_price"

instance ToJSON StepUp where
    toJSON (StepUp the1502StepUp) =
        object
        [ "15-02" .= the1502StepUp
        ]

instance FromJSON StepUp where
    parseJSON (Object v) = StepUp
        <$> v .:? "15-02"
