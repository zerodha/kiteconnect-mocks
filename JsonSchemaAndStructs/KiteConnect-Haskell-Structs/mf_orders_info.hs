{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( MFOrdersInfo (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , Amount (..)
    , LastPriceDate (..)
    , MFOrdersInfoClass (..)
    , MFOrdersInfoProperties (..)
    , DataClass (..)
    , Type (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFOrdersInfo = MFOrdersInfo
    { refMFOrdersInfo :: Text
    , schemaMFOrdersInfo :: Text
    , definitionsMFOrdersInfo :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , mfOrdersInfoDefinitions :: MFOrdersInfoClass
    } deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { amountDataProperties :: Amount
    , averagePriceDataProperties :: Amount
    , exchangeOrderIDDataProperties :: Amount
    , exchangeTimestampDataProperties :: Amount
    , folioDataProperties :: Amount
    , fundDataProperties :: Amount
    , lastPriceDataProperties :: Amount
    , lastPriceDateDataProperties :: LastPriceDate
    , orderIDDataProperties :: LastPriceDate
    , orderTimestampDataProperties :: LastPriceDate
    , placedByDataProperties :: Amount
    , purchaseTypeDataProperties :: Amount
    , quantityDataProperties :: Amount
    , settlementIDDataProperties :: Amount
    , statusDataProperties :: Amount
    , statusMessageDataProperties :: Amount
    , tagDataProperties :: Amount
    , tradingsymbolDataProperties :: Amount
    , transactionTypeDataProperties :: Amount
    , varietyDataProperties :: Amount
    } deriving (Show)

data Amount = Amount
    { amountTypeAmount :: Type
    } deriving (Show)

data Type
    = IntegerType
    | NullType
    | NumberType
    | StringType
    deriving (Show)

data LastPriceDate = LastPriceDate
    { formatLastPriceDate :: Text
    , lastPriceDateTypeLastPriceDate :: Type
    } deriving (Show)

data MFOrdersInfoClass = MFOrdersInfoClass
    { additionalPropertiesMFOrdersInfoClass :: Bool
    , propertiesMFOrdersInfoClass :: MFOrdersInfoProperties
    , requiredMFOrdersInfoClass :: Vector Text
    , titleMFOrdersInfoClass :: Text
    , mfOrdersInfoClassTypeMFOrdersInfoClass :: Text
    } deriving (Show)

data MFOrdersInfoProperties = MFOrdersInfoProperties
    { mfOrdersInfoPropertiesDataMFOrdersInfoProperties :: DataClass
    , statusMFOrdersInfoProperties :: Amount
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFOrdersInfo
decodeTopLevel = decode

instance ToJSON MFOrdersInfo where
    toJSON (MFOrdersInfo refMFOrdersInfo schemaMFOrdersInfo definitionsMFOrdersInfo) =
        object
        [ "$ref" .= refMFOrdersInfo
        , "$schema" .= schemaMFOrdersInfo
        , "definitions" .= definitionsMFOrdersInfo
        ]

instance FromJSON MFOrdersInfo where
    parseJSON (Object v) = MFOrdersInfo
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions mfOrdersInfoDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "MFOrdersInfo" .= mfOrdersInfoDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "MFOrdersInfo"

instance ToJSON Data where
    toJSON (Data additionalPropertiesData propertiesData requiredData titleData dataTypeData) =
        object
        [ "additionalProperties" .= additionalPropertiesData
        , "properties" .= propertiesData
        , "required" .= requiredData
        , "title" .= titleData
        , "type" .= dataTypeData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON DataProperties where
    toJSON (DataProperties amountDataProperties averagePriceDataProperties exchangeOrderIDDataProperties exchangeTimestampDataProperties folioDataProperties fundDataProperties lastPriceDataProperties lastPriceDateDataProperties orderIDDataProperties orderTimestampDataProperties placedByDataProperties purchaseTypeDataProperties quantityDataProperties settlementIDDataProperties statusDataProperties statusMessageDataProperties tagDataProperties tradingsymbolDataProperties transactionTypeDataProperties varietyDataProperties) =
        object
        [ "amount" .= amountDataProperties
        , "average_price" .= averagePriceDataProperties
        , "exchange_order_id" .= exchangeOrderIDDataProperties
        , "exchange_timestamp" .= exchangeTimestampDataProperties
        , "folio" .= folioDataProperties
        , "fund" .= fundDataProperties
        , "last_price" .= lastPriceDataProperties
        , "last_price_date" .= lastPriceDateDataProperties
        , "order_id" .= orderIDDataProperties
        , "order_timestamp" .= orderTimestampDataProperties
        , "placed_by" .= placedByDataProperties
        , "purchase_type" .= purchaseTypeDataProperties
        , "quantity" .= quantityDataProperties
        , "settlement_id" .= settlementIDDataProperties
        , "status" .= statusDataProperties
        , "status_message" .= statusMessageDataProperties
        , "tag" .= tagDataProperties
        , "tradingsymbol" .= tradingsymbolDataProperties
        , "transaction_type" .= transactionTypeDataProperties
        , "variety" .= varietyDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "amount"
        <*> v .: "average_price"
        <*> v .: "exchange_order_id"
        <*> v .: "exchange_timestamp"
        <*> v .: "folio"
        <*> v .: "fund"
        <*> v .: "last_price"
        <*> v .: "last_price_date"
        <*> v .: "order_id"
        <*> v .: "order_timestamp"
        <*> v .: "placed_by"
        <*> v .: "purchase_type"
        <*> v .: "quantity"
        <*> v .: "settlement_id"
        <*> v .: "status"
        <*> v .: "status_message"
        <*> v .: "tag"
        <*> v .: "tradingsymbol"
        <*> v .: "transaction_type"
        <*> v .: "variety"

instance ToJSON Amount where
    toJSON (Amount amountTypeAmount) =
        object
        [ "type" .= amountTypeAmount
        ]

instance FromJSON Amount where
    parseJSON (Object v) = Amount
        <$> v .: "type"

instance ToJSON Type where
    toJSON IntegerType = "integer"
    toJSON NullType = "null"
    toJSON NumberType = "number"
    toJSON StringType = "string"

instance FromJSON Type where
    parseJSON = withText "Type" parseText
        where
            parseText "integer" = return IntegerType
            parseText "null" = return NullType
            parseText "number" = return NumberType
            parseText "string" = return StringType

instance ToJSON LastPriceDate where
    toJSON (LastPriceDate formatLastPriceDate lastPriceDateTypeLastPriceDate) =
        object
        [ "format" .= formatLastPriceDate
        , "type" .= lastPriceDateTypeLastPriceDate
        ]

instance FromJSON LastPriceDate where
    parseJSON (Object v) = LastPriceDate
        <$> v .: "format"
        <*> v .: "type"

instance ToJSON MFOrdersInfoClass where
    toJSON (MFOrdersInfoClass additionalPropertiesMFOrdersInfoClass propertiesMFOrdersInfoClass requiredMFOrdersInfoClass titleMFOrdersInfoClass mfOrdersInfoClassTypeMFOrdersInfoClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMFOrdersInfoClass
        , "properties" .= propertiesMFOrdersInfoClass
        , "required" .= requiredMFOrdersInfoClass
        , "title" .= titleMFOrdersInfoClass
        , "type" .= mfOrdersInfoClassTypeMFOrdersInfoClass
        ]

instance FromJSON MFOrdersInfoClass where
    parseJSON (Object v) = MFOrdersInfoClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MFOrdersInfoProperties where
    toJSON (MFOrdersInfoProperties mfOrdersInfoPropertiesDataMFOrdersInfoProperties statusMFOrdersInfoProperties) =
        object
        [ "data" .= mfOrdersInfoPropertiesDataMFOrdersInfoProperties
        , "status" .= statusMFOrdersInfoProperties
        ]

instance FromJSON MFOrdersInfoProperties where
    parseJSON (Object v) = MFOrdersInfoProperties
        <$> v .: "data"
        <*> v .: "status"

instance ToJSON DataClass where
    toJSON (DataClass refDataClass) =
        object
        [ "$ref" .= refDataClass
        ]

instance FromJSON DataClass where
    parseJSON (Object v) = DataClass
        <$> v .: "$ref"
