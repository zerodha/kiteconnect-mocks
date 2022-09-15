{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( OrderInfo (..)
    , Definitions (..)
    , Datum (..)
    , DatumProperties (..)
    , AveragePrice (..)
    , ExchangeOrderID (..)
    , ExchangeTimestamp (..)
    , OrderTimestamp (..)
    , OrderInfoClass (..)
    , OrderInfoProperties (..)
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

data OrderInfo = OrderInfo
    { refOrderInfo :: Text
    , schemaOrderInfo :: Text
    , definitionsOrderInfo :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { datumDefinitions :: Datum
    , orderInfoDefinitions :: OrderInfoClass
    } deriving (Show)

data Datum = Datum
    { additionalPropertiesDatum :: Bool
    , propertiesDatum :: DatumProperties
    , requiredDatum :: Vector Text
    , titleDatum :: Text
    , datumTypeDatum :: Text
    } deriving (Show)

data DatumProperties = DatumProperties
    { averagePriceDatumProperties :: AveragePrice
    , cancelledQuantityDatumProperties :: AveragePrice
    , disclosedQuantityDatumProperties :: AveragePrice
    , exchangeDatumProperties :: AveragePrice
    , exchangeOrderIDDatumProperties :: ExchangeOrderID
    , exchangeTimestampDatumProperties :: ExchangeTimestamp
    , filledQuantityDatumProperties :: AveragePrice
    , instrumentTokenDatumProperties :: AveragePrice
    , orderIDDatumProperties :: AveragePrice
    , orderTimestampDatumProperties :: OrderTimestamp
    , orderTypeDatumProperties :: AveragePrice
    , parentOrderIDDatumProperties :: AveragePrice
    , pendingQuantityDatumProperties :: AveragePrice
    , placedByDatumProperties :: AveragePrice
    , priceDatumProperties :: AveragePrice
    , productDatumProperties :: AveragePrice
    , quantityDatumProperties :: AveragePrice
    , statusDatumProperties :: AveragePrice
    , statusMessageDatumProperties :: AveragePrice
    , tagDatumProperties :: AveragePrice
    , tradingsymbolDatumProperties :: AveragePrice
    , transactionTypeDatumProperties :: AveragePrice
    , triggerPriceDatumProperties :: AveragePrice
    , validityDatumProperties :: AveragePrice
    , varietyDatumProperties :: AveragePrice
    } deriving (Show)

data AveragePrice = AveragePrice
    { averagePriceTypeAveragePrice :: Type
    } deriving (Show)

data Type
    = IntegerType
    | NullType
    | NumberType
    | StringType
    deriving (Show)

data ExchangeOrderID = ExchangeOrderID
    { anyOfExchangeOrderID :: Vector AveragePrice
    } deriving (Show)

data ExchangeTimestamp = ExchangeTimestamp
    { anyOfExchangeTimestamp :: Vector OrderTimestamp
    } deriving (Show)

data OrderTimestamp = OrderTimestamp
    { formatOrderTimestamp :: Maybe Text
    , orderTimestampTypeOrderTimestamp :: Type
    } deriving (Show)

data OrderInfoClass = OrderInfoClass
    { additionalPropertiesOrderInfoClass :: Bool
    , propertiesOrderInfoClass :: OrderInfoProperties
    , requiredOrderInfoClass :: Vector Text
    , titleOrderInfoClass :: Text
    , orderInfoClassTypeOrderInfoClass :: Text
    } deriving (Show)

data OrderInfoProperties = OrderInfoProperties
    { orderInfoPropertiesDataOrderInfoProperties :: Data
    , statusOrderInfoProperties :: AveragePrice
    } deriving (Show)

data Data = Data
    { itemsData :: Items
    , dataTypeData :: Text
    } deriving (Show)

data Items = Items
    { refItems :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe OrderInfo
decodeTopLevel = decode

instance ToJSON OrderInfo where
    toJSON (OrderInfo refOrderInfo schemaOrderInfo definitionsOrderInfo) =
        object
        [ "$ref" .= refOrderInfo
        , "$schema" .= schemaOrderInfo
        , "definitions" .= definitionsOrderInfo
        ]

instance FromJSON OrderInfo where
    parseJSON (Object v) = OrderInfo
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions datumDefinitions orderInfoDefinitions) =
        object
        [ "Datum" .= datumDefinitions
        , "OrderInfo" .= orderInfoDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Datum"
        <*> v .: "OrderInfo"

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
    toJSON (DatumProperties averagePriceDatumProperties cancelledQuantityDatumProperties disclosedQuantityDatumProperties exchangeDatumProperties exchangeOrderIDDatumProperties exchangeTimestampDatumProperties filledQuantityDatumProperties instrumentTokenDatumProperties orderIDDatumProperties orderTimestampDatumProperties orderTypeDatumProperties parentOrderIDDatumProperties pendingQuantityDatumProperties placedByDatumProperties priceDatumProperties productDatumProperties quantityDatumProperties statusDatumProperties statusMessageDatumProperties tagDatumProperties tradingsymbolDatumProperties transactionTypeDatumProperties triggerPriceDatumProperties validityDatumProperties varietyDatumProperties) =
        object
        [ "average_price" .= averagePriceDatumProperties
        , "cancelled_quantity" .= cancelledQuantityDatumProperties
        , "disclosed_quantity" .= disclosedQuantityDatumProperties
        , "exchange" .= exchangeDatumProperties
        , "exchange_order_id" .= exchangeOrderIDDatumProperties
        , "exchange_timestamp" .= exchangeTimestampDatumProperties
        , "filled_quantity" .= filledQuantityDatumProperties
        , "instrument_token" .= instrumentTokenDatumProperties
        , "order_id" .= orderIDDatumProperties
        , "order_timestamp" .= orderTimestampDatumProperties
        , "order_type" .= orderTypeDatumProperties
        , "parent_order_id" .= parentOrderIDDatumProperties
        , "pending_quantity" .= pendingQuantityDatumProperties
        , "placed_by" .= placedByDatumProperties
        , "price" .= priceDatumProperties
        , "product" .= productDatumProperties
        , "quantity" .= quantityDatumProperties
        , "status" .= statusDatumProperties
        , "status_message" .= statusMessageDatumProperties
        , "tag" .= tagDatumProperties
        , "tradingsymbol" .= tradingsymbolDatumProperties
        , "transaction_type" .= transactionTypeDatumProperties
        , "trigger_price" .= triggerPriceDatumProperties
        , "validity" .= validityDatumProperties
        , "variety" .= varietyDatumProperties
        ]

instance FromJSON DatumProperties where
    parseJSON (Object v) = DatumProperties
        <$> v .: "average_price"
        <*> v .: "cancelled_quantity"
        <*> v .: "disclosed_quantity"
        <*> v .: "exchange"
        <*> v .: "exchange_order_id"
        <*> v .: "exchange_timestamp"
        <*> v .: "filled_quantity"
        <*> v .: "instrument_token"
        <*> v .: "order_id"
        <*> v .: "order_timestamp"
        <*> v .: "order_type"
        <*> v .: "parent_order_id"
        <*> v .: "pending_quantity"
        <*> v .: "placed_by"
        <*> v .: "price"
        <*> v .: "product"
        <*> v .: "quantity"
        <*> v .: "status"
        <*> v .: "status_message"
        <*> v .: "tag"
        <*> v .: "tradingsymbol"
        <*> v .: "transaction_type"
        <*> v .: "trigger_price"
        <*> v .: "validity"
        <*> v .: "variety"

instance ToJSON AveragePrice where
    toJSON (AveragePrice averagePriceTypeAveragePrice) =
        object
        [ "type" .= averagePriceTypeAveragePrice
        ]

instance FromJSON AveragePrice where
    parseJSON (Object v) = AveragePrice
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

instance ToJSON ExchangeOrderID where
    toJSON (ExchangeOrderID anyOfExchangeOrderID) =
        object
        [ "anyOf" .= anyOfExchangeOrderID
        ]

instance FromJSON ExchangeOrderID where
    parseJSON (Object v) = ExchangeOrderID
        <$> v .: "anyOf"

instance ToJSON ExchangeTimestamp where
    toJSON (ExchangeTimestamp anyOfExchangeTimestamp) =
        object
        [ "anyOf" .= anyOfExchangeTimestamp
        ]

instance FromJSON ExchangeTimestamp where
    parseJSON (Object v) = ExchangeTimestamp
        <$> v .: "anyOf"

instance ToJSON OrderTimestamp where
    toJSON (OrderTimestamp formatOrderTimestamp orderTimestampTypeOrderTimestamp) =
        object
        [ "format" .= formatOrderTimestamp
        , "type" .= orderTimestampTypeOrderTimestamp
        ]

instance FromJSON OrderTimestamp where
    parseJSON (Object v) = OrderTimestamp
        <$> v .:? "format"
        <*> v .: "type"

instance ToJSON OrderInfoClass where
    toJSON (OrderInfoClass additionalPropertiesOrderInfoClass propertiesOrderInfoClass requiredOrderInfoClass titleOrderInfoClass orderInfoClassTypeOrderInfoClass) =
        object
        [ "additionalProperties" .= additionalPropertiesOrderInfoClass
        , "properties" .= propertiesOrderInfoClass
        , "required" .= requiredOrderInfoClass
        , "title" .= titleOrderInfoClass
        , "type" .= orderInfoClassTypeOrderInfoClass
        ]

instance FromJSON OrderInfoClass where
    parseJSON (Object v) = OrderInfoClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON OrderInfoProperties where
    toJSON (OrderInfoProperties orderInfoPropertiesDataOrderInfoProperties statusOrderInfoProperties) =
        object
        [ "data" .= orderInfoPropertiesDataOrderInfoProperties
        , "status" .= statusOrderInfoProperties
        ]

instance FromJSON OrderInfoProperties where
    parseJSON (Object v) = OrderInfoProperties
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
