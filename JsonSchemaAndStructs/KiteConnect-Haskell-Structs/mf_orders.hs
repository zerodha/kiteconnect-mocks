{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( MFOrders (..)
    , Definitions (..)
    , Datum (..)
    , DatumProperties (..)
    , Amount (..)
    , ExchangeOrderID (..)
    , LastPriceDate (..)
    , Tag (..)
    , MFOrdersClass (..)
    , MFOrdersProperties (..)
    , Data (..)
    , Items (..)
    , Type (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFOrders = MFOrders
    { refMFOrders :: Text
    , schemaMFOrders :: Text
    , definitionsMFOrders :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { datumDefinitions :: Datum
    , mfOrdersDefinitions :: MFOrdersClass
    } deriving (Show)

data Datum = Datum
    { additionalPropertiesDatum :: Bool
    , propertiesDatum :: DatumProperties
    , requiredDatum :: Vector Text
    , titleDatum :: Text
    , datumTypeDatum :: Text
    } deriving (Show)

data DatumProperties = DatumProperties
    { amountDatumProperties :: Amount
    , averagePriceDatumProperties :: Amount
    , exchangeOrderIDDatumProperties :: ExchangeOrderID
    , exchangeTimestampDatumProperties :: ExchangeOrderID
    , folioDatumProperties :: Amount
    , fundDatumProperties :: Amount
    , lastPriceDatumProperties :: Amount
    , lastPriceDateDatumProperties :: LastPriceDate
    , orderIDDatumProperties :: LastPriceDate
    , orderTimestampDatumProperties :: LastPriceDate
    , placedByDatumProperties :: Amount
    , purchaseTypeDatumProperties :: Amount
    , quantityDatumProperties :: Amount
    , settlementIDDatumProperties :: ExchangeOrderID
    , statusDatumProperties :: Amount
    , statusMessageDatumProperties :: Amount
    , tagDatumProperties :: Tag
    , tradingsymbolDatumProperties :: Amount
    , transactionTypeDatumProperties :: Amount
    , varietyDatumProperties :: Amount
    } deriving (Show)

data Amount = Amount
    { amountTypeAmount :: Type
    } deriving (Show)

data Type
    = NullType
    | NumberType
    | StringType
    deriving (Show)

data ExchangeOrderID = ExchangeOrderID
    { anyOfExchangeOrderID :: Vector LastPriceDate
    } deriving (Show)

data LastPriceDate = LastPriceDate
    { formatLastPriceDate :: Maybe Text
    , lastPriceDateTypeLastPriceDate :: Type
    } deriving (Show)

data Tag = Tag
    { anyOfTag :: Vector Amount
    } deriving (Show)

data MFOrdersClass = MFOrdersClass
    { additionalPropertiesMFOrdersClass :: Bool
    , propertiesMFOrdersClass :: MFOrdersProperties
    , requiredMFOrdersClass :: Vector Text
    , titleMFOrdersClass :: Text
    , mfOrdersClassTypeMFOrdersClass :: Text
    } deriving (Show)

data MFOrdersProperties = MFOrdersProperties
    { mfOrdersPropertiesDataMFOrdersProperties :: Data
    , statusMFOrdersProperties :: Amount
    } deriving (Show)

data Data = Data
    { itemsData :: Items
    , dataTypeData :: Text
    } deriving (Show)

data Items = Items
    { refItems :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFOrders
decodeTopLevel = decode

instance ToJSON MFOrders where
    toJSON (MFOrders refMFOrders schemaMFOrders definitionsMFOrders) =
        object
        [ "$ref" .= refMFOrders
        , "$schema" .= schemaMFOrders
        , "definitions" .= definitionsMFOrders
        ]

instance FromJSON MFOrders where
    parseJSON (Object v) = MFOrders
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions datumDefinitions mfOrdersDefinitions) =
        object
        [ "Datum" .= datumDefinitions
        , "MFOrders" .= mfOrdersDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Datum"
        <*> v .: "MFOrders"

instance ToJSON Datum where
    toJSON (Datum additionalPropertiesDatum propertiesDatum requiredDatum titleDatum datumTypeDatum) =
        object
        [ "additionalProperties" .= additionalPropertiesDatum
        , "properties" .= propertiesDatum
        , "required" .= requiredDatum
        , "title" .= titleDatum
        , "type" .= datumTypeDatum
        ]

instance FromJSON Datum where
    parseJSON (Object v) = Datum
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON DatumProperties where
    toJSON (DatumProperties amountDatumProperties averagePriceDatumProperties exchangeOrderIDDatumProperties exchangeTimestampDatumProperties folioDatumProperties fundDatumProperties lastPriceDatumProperties lastPriceDateDatumProperties orderIDDatumProperties orderTimestampDatumProperties placedByDatumProperties purchaseTypeDatumProperties quantityDatumProperties settlementIDDatumProperties statusDatumProperties statusMessageDatumProperties tagDatumProperties tradingsymbolDatumProperties transactionTypeDatumProperties varietyDatumProperties) =
        object
        [ "amount" .= amountDatumProperties
        , "average_price" .= averagePriceDatumProperties
        , "exchange_order_id" .= exchangeOrderIDDatumProperties
        , "exchange_timestamp" .= exchangeTimestampDatumProperties
        , "folio" .= folioDatumProperties
        , "fund" .= fundDatumProperties
        , "last_price" .= lastPriceDatumProperties
        , "last_price_date" .= lastPriceDateDatumProperties
        , "order_id" .= orderIDDatumProperties
        , "order_timestamp" .= orderTimestampDatumProperties
        , "placed_by" .= placedByDatumProperties
        , "purchase_type" .= purchaseTypeDatumProperties
        , "quantity" .= quantityDatumProperties
        , "settlement_id" .= settlementIDDatumProperties
        , "status" .= statusDatumProperties
        , "status_message" .= statusMessageDatumProperties
        , "tag" .= tagDatumProperties
        , "tradingsymbol" .= tradingsymbolDatumProperties
        , "transaction_type" .= transactionTypeDatumProperties
        , "variety" .= varietyDatumProperties
        ]

instance FromJSON DatumProperties where
    parseJSON (Object v) = DatumProperties
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
    toJSON NullType = "null"
    toJSON NumberType = "number"
    toJSON StringType = "string"

instance FromJSON Type where
    parseJSON = withText "Type" parseText
        where
            parseText "null" = return NullType
            parseText "number" = return NumberType
            parseText "string" = return StringType

instance ToJSON ExchangeOrderID where
    toJSON (ExchangeOrderID anyOfExchangeOrderID) =
        object
        [ "anyOf" .= anyOfExchangeOrderID
        ]

instance FromJSON ExchangeOrderID where
    parseJSON (Object v) = ExchangeOrderID
        <$> v .: "anyOf"

instance ToJSON LastPriceDate where
    toJSON (LastPriceDate formatLastPriceDate lastPriceDateTypeLastPriceDate) =
        object
        [ "format" .= formatLastPriceDate
        , "type" .= lastPriceDateTypeLastPriceDate
        ]

instance FromJSON LastPriceDate where
    parseJSON (Object v) = LastPriceDate
        <$> v .:? "format"
        <*> v .: "type"

instance ToJSON Tag where
    toJSON (Tag anyOfTag) =
        object
        [ "anyOf" .= anyOfTag
        ]

instance FromJSON Tag where
    parseJSON (Object v) = Tag
        <$> v .: "anyOf"

instance ToJSON MFOrdersClass where
    toJSON (MFOrdersClass additionalPropertiesMFOrdersClass propertiesMFOrdersClass requiredMFOrdersClass titleMFOrdersClass mfOrdersClassTypeMFOrdersClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMFOrdersClass
        , "properties" .= propertiesMFOrdersClass
        , "required" .= requiredMFOrdersClass
        , "title" .= titleMFOrdersClass
        , "type" .= mfOrdersClassTypeMFOrdersClass
        ]

instance FromJSON MFOrdersClass where
    parseJSON (Object v) = MFOrdersClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MFOrdersProperties where
    toJSON (MFOrdersProperties mfOrdersPropertiesDataMFOrdersProperties statusMFOrdersProperties) =
        object
        [ "data" .= mfOrdersPropertiesDataMFOrdersProperties
        , "status" .= statusMFOrdersProperties
        ]

instance FromJSON MFOrdersProperties where
    parseJSON (Object v) = MFOrdersProperties
        <$> v .: "data"
        <*> v .: "status"

instance ToJSON Data where
    toJSON (Data itemsData dataTypeData) =
        object
        [ "items" .= itemsData
        , "type" .= dataTypeData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "items"
        <*> v .: "type"

instance ToJSON Items where
    toJSON (Items refItems) =
        object
        [ "$ref" .= refItems
        ]

instance FromJSON Items where
    parseJSON (Object v) = Items
        <$> v .: "$ref"
