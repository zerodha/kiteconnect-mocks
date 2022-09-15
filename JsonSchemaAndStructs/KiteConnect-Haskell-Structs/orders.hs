{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( Orders (..)
    , Definitions (..)
    , Datum (..)
    , DatumProperties (..)
    , AveragePrice (..)
    , ExchangeOrderID (..)
    , ExchangeETimestamp (..)
    , OrderTimestamp (..)
    , Meta (..)
    , Tags (..)
    , Iceberg (..)
    , IcebergProperties (..)
    , MetaClass (..)
    , MetaProperties (..)
    , OrdersClass (..)
    , OrdersProperties (..)
    , Data (..)
    , Type (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Orders = Orders
    { refOrders :: Text
    , schemaOrders :: Text
    , definitionsOrders :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { datumDefinitions :: Datum
    , icebergDefinitions :: Iceberg
    , metaDefinitions :: MetaClass
    , ordersDefinitions :: OrdersClass
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
    , exchangeTimestampDatumProperties :: ExchangeETimestamp
    , exchangeUpdateTimestampDatumProperties :: ExchangeETimestamp
    , filledQuantityDatumProperties :: AveragePrice
    , guidDatumProperties :: AveragePrice
    , instrumentTokenDatumProperties :: AveragePrice
    , marketProtectionDatumProperties :: AveragePrice
    , metaDatumProperties :: Meta
    , modifiedDatumProperties :: AveragePrice
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
    , statusMessageDatumProperties :: ExchangeOrderID
    , statusMessageRawDatumProperties :: ExchangeOrderID
    , tagDatumProperties :: ExchangeOrderID
    , tagsDatumProperties :: Tags
    , tradingsymbolDatumProperties :: AveragePrice
    , transactionTypeDatumProperties :: AveragePrice
    , triggerPriceDatumProperties :: AveragePrice
    , validityDatumProperties :: AveragePrice
    , validityTTLDatumProperties :: AveragePrice
    , varietyDatumProperties :: AveragePrice
    } deriving (Show)

data AveragePrice = AveragePrice
    { averagePriceTypeAveragePrice :: Type
    } deriving (Show)

data Type
    = BooleanType
    | IntegerType
    | NullType
    | StringType
    deriving (Show)

data ExchangeOrderID = ExchangeOrderID
    { anyOfExchangeOrderID :: Vector AveragePrice
    } deriving (Show)

data ExchangeETimestamp = ExchangeETimestamp
    { anyOfExchangeETimestamp :: Vector OrderTimestamp
    } deriving (Show)

data OrderTimestamp = OrderTimestamp
    { formatOrderTimestamp :: Maybe Text
    , orderTimestampTypeOrderTimestamp :: Type
    } deriving (Show)

data Meta = Meta
    { refMeta :: Text
    } deriving (Show)

data Tags = Tags
    { itemsTags :: AveragePrice
    , tagsTypeTags :: Text
    } deriving (Show)

data Iceberg = Iceberg
    { additionalPropertiesIceberg :: Bool
    , propertiesIceberg :: IcebergProperties
    , requiredIceberg :: Vector Text
    , titleIceberg :: Text
    , icebergTypeIceberg :: Text
    } deriving (Show)

data IcebergProperties = IcebergProperties
    { legIcebergProperties :: AveragePrice
    , legQuantityIcebergProperties :: AveragePrice
    , legsIcebergProperties :: AveragePrice
    , remainingQuantityIcebergProperties :: AveragePrice
    , totalQuantityIcebergProperties :: AveragePrice
    } deriving (Show)

data MetaClass = MetaClass
    { additionalPropertiesMetaClass :: Bool
    , propertiesMetaClass :: MetaProperties
    , requiredMetaClass :: Vector (Maybe Text)
    , titleMetaClass :: Text
    , metaClassTypeMetaClass :: Text
    } deriving (Show)

data MetaProperties = MetaProperties
    { icebergMetaProperties :: Meta
    } deriving (Show)

data OrdersClass = OrdersClass
    { additionalPropertiesOrdersClass :: Bool
    , propertiesOrdersClass :: OrdersProperties
    , requiredOrdersClass :: Vector Text
    , titleOrdersClass :: Text
    , ordersClassTypeOrdersClass :: Text
    } deriving (Show)

data OrdersProperties = OrdersProperties
    { ordersPropertiesDataOrdersProperties :: Data
    , statusOrdersProperties :: AveragePrice
    } deriving (Show)

data Data = Data
    { itemsData :: Meta
    , dataTypeData :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Orders
decodeTopLevel = decode

instance ToJSON Orders where
    toJSON (Orders refOrders schemaOrders definitionsOrders) =
        object
        [ "$ref" .= refOrders
        , "$schema" .= schemaOrders
        , "definitions" .= definitionsOrders
        ]

instance FromJSON Orders where
    parseJSON (Object v) = Orders
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions datumDefinitions icebergDefinitions metaDefinitions ordersDefinitions) =
        object
        [ "Datum" .= datumDefinitions
        , "Iceberg" .= icebergDefinitions
        , "Meta" .= metaDefinitions
        , "Orders" .= ordersDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Datum"
        <*> v .: "Iceberg"
        <*> v .: "Meta"
        <*> v .: "Orders"

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
    toJSON (DatumProperties averagePriceDatumProperties cancelledQuantityDatumProperties disclosedQuantityDatumProperties exchangeDatumProperties exchangeOrderIDDatumProperties exchangeTimestampDatumProperties exchangeUpdateTimestampDatumProperties filledQuantityDatumProperties guidDatumProperties instrumentTokenDatumProperties marketProtectionDatumProperties metaDatumProperties modifiedDatumProperties orderIDDatumProperties orderTimestampDatumProperties orderTypeDatumProperties parentOrderIDDatumProperties pendingQuantityDatumProperties placedByDatumProperties priceDatumProperties productDatumProperties quantityDatumProperties statusDatumProperties statusMessageDatumProperties statusMessageRawDatumProperties tagDatumProperties tagsDatumProperties tradingsymbolDatumProperties transactionTypeDatumProperties triggerPriceDatumProperties validityDatumProperties validityTTLDatumProperties varietyDatumProperties) =
        object
        [ "average_price" .= averagePriceDatumProperties
        , "cancelled_quantity" .= cancelledQuantityDatumProperties
        , "disclosed_quantity" .= disclosedQuantityDatumProperties
        , "exchange" .= exchangeDatumProperties
        , "exchange_order_id" .= exchangeOrderIDDatumProperties
        , "exchange_timestamp" .= exchangeTimestampDatumProperties
        , "exchange_update_timestamp" .= exchangeUpdateTimestampDatumProperties
        , "filled_quantity" .= filledQuantityDatumProperties
        , "guid" .= guidDatumProperties
        , "instrument_token" .= instrumentTokenDatumProperties
        , "market_protection" .= marketProtectionDatumProperties
        , "meta" .= metaDatumProperties
        , "modified" .= modifiedDatumProperties
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
        , "status_message_raw" .= statusMessageRawDatumProperties
        , "tag" .= tagDatumProperties
        , "tags" .= tagsDatumProperties
        , "tradingsymbol" .= tradingsymbolDatumProperties
        , "transaction_type" .= transactionTypeDatumProperties
        , "trigger_price" .= triggerPriceDatumProperties
        , "validity" .= validityDatumProperties
        , "validity_ttl" .= validityTTLDatumProperties
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
        <*> v .: "exchange_update_timestamp"
        <*> v .: "filled_quantity"
        <*> v .: "guid"
        <*> v .: "instrument_token"
        <*> v .: "market_protection"
        <*> v .: "meta"
        <*> v .: "modified"
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
        <*> v .: "status_message_raw"
        <*> v .: "tag"
        <*> v .: "tags"
        <*> v .: "tradingsymbol"
        <*> v .: "transaction_type"
        <*> v .: "trigger_price"
        <*> v .: "validity"
        <*> v .: "validity_ttl"
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
    toJSON BooleanType = "boolean"
    toJSON IntegerType = "integer"
    toJSON NullType = "null"
    toJSON StringType = "string"

instance FromJSON Type where
    parseJSON = withText "Type" parseText
        where
            parseText "boolean" = return BooleanType
            parseText "integer" = return IntegerType
            parseText "null" = return NullType
            parseText "string" = return StringType

instance ToJSON ExchangeOrderID where
    toJSON (ExchangeOrderID anyOfExchangeOrderID) =
        object
        [ "anyOf" .= anyOfExchangeOrderID
        ]

instance FromJSON ExchangeOrderID where
    parseJSON (Object v) = ExchangeOrderID
        <$> v .: "anyOf"

instance ToJSON ExchangeETimestamp where
    toJSON (ExchangeETimestamp anyOfExchangeETimestamp) =
        object
        [ "anyOf" .= anyOfExchangeETimestamp
        ]

instance FromJSON ExchangeETimestamp where
    parseJSON (Object v) = ExchangeETimestamp
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

instance ToJSON Meta where
    toJSON (Meta refMeta) =
        object
        [ "$ref" .= refMeta
        ]

instance FromJSON Meta where
    parseJSON (Object v) = Meta
        <$> v .: "$ref"

instance ToJSON Tags where
    toJSON (Tags itemsTags tagsTypeTags) =
        object
        [ "items" .= itemsTags
        , "type" .= tagsTypeTags
        ]

instance FromJSON Tags where
    parseJSON (Object v) = Tags
        <$> v .: "items"
        <*> v .: "type"

instance ToJSON Iceberg where
    toJSON (Iceberg additionalPropertiesIceberg propertiesIceberg requiredIceberg titleIceberg icebergTypeIceberg) =
        object
        [ "additionalProperties" .= additionalPropertiesIceberg
        , "properties" .= propertiesIceberg
        , "required" .= requiredIceberg
        , "title" .= titleIceberg
        , "type" .= icebergTypeIceberg
        ]

instance FromJSON Iceberg where
    parseJSON (Object v) = Iceberg
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON IcebergProperties where
    toJSON (IcebergProperties legIcebergProperties legQuantityIcebergProperties legsIcebergProperties remainingQuantityIcebergProperties totalQuantityIcebergProperties) =
        object
        [ "leg" .= legIcebergProperties
        , "leg_quantity" .= legQuantityIcebergProperties
        , "legs" .= legsIcebergProperties
        , "remaining_quantity" .= remainingQuantityIcebergProperties
        , "total_quantity" .= totalQuantityIcebergProperties
        ]

instance FromJSON IcebergProperties where
    parseJSON (Object v) = IcebergProperties
        <$> v .: "leg"
        <*> v .: "leg_quantity"
        <*> v .: "legs"
        <*> v .: "remaining_quantity"
        <*> v .: "total_quantity"

instance ToJSON MetaClass where
    toJSON (MetaClass additionalPropertiesMetaClass propertiesMetaClass requiredMetaClass titleMetaClass metaClassTypeMetaClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMetaClass
        , "properties" .= propertiesMetaClass
        , "required" .= requiredMetaClass
        , "title" .= titleMetaClass
        , "type" .= metaClassTypeMetaClass
        ]

instance FromJSON MetaClass where
    parseJSON (Object v) = MetaClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MetaProperties where
    toJSON (MetaProperties icebergMetaProperties) =
        object
        [ "iceberg" .= icebergMetaProperties
        ]

instance FromJSON MetaProperties where
    parseJSON (Object v) = MetaProperties
        <$> v .: "iceberg"

instance ToJSON OrdersClass where
    toJSON (OrdersClass additionalPropertiesOrdersClass propertiesOrdersClass requiredOrdersClass titleOrdersClass ordersClassTypeOrdersClass) =
        object
        [ "additionalProperties" .= additionalPropertiesOrdersClass
        , "properties" .= propertiesOrdersClass
        , "required" .= requiredOrdersClass
        , "title" .= titleOrdersClass
        , "type" .= ordersClassTypeOrdersClass
        ]

instance FromJSON OrdersClass where
    parseJSON (Object v) = OrdersClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON OrdersProperties where
    toJSON (OrdersProperties ordersPropertiesDataOrdersProperties statusOrdersProperties) =
        object
        [ "data" .= ordersPropertiesDataOrdersProperties
        , "status" .= statusOrdersProperties
        ]

instance FromJSON OrdersProperties where
    parseJSON (Object v) = OrdersProperties
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
