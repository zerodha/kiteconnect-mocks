{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MfSips
    ( MFSips (..)
    , Datum (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFSips = MFSips
    { mfSipsDataMFSips :: Maybe (Vector Datum)
    } deriving (Show)

data Datum = Datum
    { completedInstalmentsDatum :: Maybe Int
    , createdDatum :: Maybe Text
    , dividendTypeDatum :: Maybe Text
    , frequencyDatum :: Maybe Text
    , fundDatum :: Maybe Text
    , instalmentAmountDatum :: Maybe Int
    , instalmentDayDatum :: Maybe Int
    , instalmentsDatum :: Maybe Int
    , lastInstalmentDatum :: Maybe Text
    , nextInstalmentDatum :: Maybe Text
    , pendingInstalmentsDatum :: Maybe Int
    , sipIDDatum :: Maybe Text
    , sipRegNumDatum :: Maybe Text
    , sipTypeDatum :: Maybe Text
    , statusDatum :: Maybe Text
    , stepUpDatum :: Maybe (HashMap Text Int)
    , tagDatum :: Maybe Text
    , tradingsymbolDatum :: Maybe Text
    , transactionTypeDatum :: Maybe Text
    , triggerPriceDatum :: Maybe Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFSips
decodeTopLevel = decode

instance ToJSON MFSips where
    toJSON (MFSips mfSipsDataMFSips) =
        object
        [ "data" .= mfSipsDataMFSips
        ]

instance FromJSON MFSips where
    parseJSON (Object v) = MFSips
        <$> v .:? "data"

instance ToJSON Datum where
    toJSON (Datum completedInstalmentsDatum createdDatum dividendTypeDatum frequencyDatum fundDatum instalmentAmountDatum instalmentDayDatum instalmentsDatum lastInstalmentDatum nextInstalmentDatum pendingInstalmentsDatum sipIDDatum sipRegNumDatum sipTypeDatum statusDatum stepUpDatum tagDatum tradingsymbolDatum transactionTypeDatum triggerPriceDatum) =
        object
        [ "completed_instalments" .= completedInstalmentsDatum
        , "created" .= createdDatum
        , "dividend_type" .= dividendTypeDatum
        , "frequency" .= frequencyDatum
        , "fund" .= fundDatum
        , "instalment_amount" .= instalmentAmountDatum
        , "instalment_day" .= instalmentDayDatum
        , "instalments" .= instalmentsDatum
        , "last_instalment" .= lastInstalmentDatum
        , "next_instalment" .= nextInstalmentDatum
        , "pending_instalments" .= pendingInstalmentsDatum
        , "sip_id" .= sipIDDatum
        , "sip_reg_num" .= sipRegNumDatum
        , "sip_type" .= sipTypeDatum
        , "status" .= statusDatum
        , "step_up" .= stepUpDatum
        , "tag" .= tagDatum
        , "tradingsymbol" .= tradingsymbolDatum
        , "transaction_type" .= transactionTypeDatum
        , "trigger_price" .= triggerPriceDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .:? "completed_instalments"
        <*> v .:? "created"
        <*> v .:? "dividend_type"
        <*> v .:? "frequency"
        <*> v .:? "fund"
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
