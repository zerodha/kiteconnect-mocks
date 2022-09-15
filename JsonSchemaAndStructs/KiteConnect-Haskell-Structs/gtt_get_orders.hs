{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( GttGetOrders (..)
    , Definitions (..)
    , Condition (..)
    , ConditionProperties (..)
    , Exchange (..)
    , TriggerValues (..)
    , Datum (..)
    , DatumProperties (..)
    , ConditionClass (..)
    , CreatedAt (..)
    , Meta (..)
    , AnyOf (..)
    , Orders (..)
    , GttGetOrdersClass (..)
    , GttGetOrdersProperties (..)
    , MetaClass (..)
    , Order (..)
    , OrderProperties (..)
    , OrderResult (..)
    , OrderResultProperties (..)
    , ResultClass (..)
    , ResultProperties (..)
    , Type (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data GttGetOrders = GttGetOrders
    { refGttGetOrders :: Text
    , schemaGttGetOrders :: Text
    , definitionsGttGetOrders :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { conditionDefinitions :: Condition
    , datumDefinitions :: Datum
    , gttGetOrdersDefinitions :: GttGetOrdersClass
    , metaDefinitions :: MetaClass
    , orderDefinitions :: Order
    , orderResultDefinitions :: OrderResult
    , resultDefinitions :: ResultClass
    } deriving (Show)

data Condition = Condition
    { additionalPropertiesCondition :: Bool
    , propertiesCondition :: ConditionProperties
    , requiredCondition :: Vector Text
    , titleCondition :: Text
    , conditionTypeCondition :: Text
    } deriving (Show)

data ConditionProperties = ConditionProperties
    { exchangeConditionProperties :: Exchange
    , instrumentTokenConditionProperties :: Exchange
    , lastPriceConditionProperties :: Exchange
    , tradingsymbolConditionProperties :: Exchange
    , triggerValuesConditionProperties :: TriggerValues
    } deriving (Show)

data Exchange = Exchange
    { exchangeTypeExchange :: Type
    } deriving (Show)

data Type
    = IntegerType
    | NullType
    | NumberType
    | StringType
    deriving (Show)

data TriggerValues = TriggerValues
    { itemsTriggerValues :: Exchange
    , triggerValuesTypeTriggerValues :: Text
    } deriving (Show)

data Datum = Datum
    { additionalPropertiesDatum :: Bool
    , propertiesDatum :: DatumProperties
    , requiredDatum :: Vector Text
    , titleDatum :: Text
    , datumTypeDatum :: Text
    } deriving (Show)

data DatumProperties = DatumProperties
    { conditionDatumProperties :: ConditionClass
    , createdAtDatumProperties :: CreatedAt
    , expiresAtDatumProperties :: CreatedAt
    , datumPropertiesIDDatumProperties :: Exchange
    , metaDatumProperties :: Meta
    , ordersDatumProperties :: Orders
    , parentTriggerDatumProperties :: Exchange
    , statusDatumProperties :: Exchange
    , datumPropertiesTypeDatumProperties :: Exchange
    , updatedAtDatumProperties :: CreatedAt
    , userIDDatumProperties :: Exchange
    } deriving (Show)

data ConditionClass = ConditionClass
    { refConditionClass :: Text
    } deriving (Show)

data CreatedAt = CreatedAt
    { formatCreatedAt :: Text
    , createdAtTypeCreatedAt :: Type
    } deriving (Show)

data Meta = Meta
    { anyOfMeta :: Vector AnyOf
    } deriving (Show)

data AnyOf = AnyOf
    { refAnyOf :: Maybe Text
    , anyOfTypeAnyOf :: Maybe Type
    } deriving (Show)

data Orders = Orders
    { itemsOrders :: ConditionClass
    , ordersTypeOrders :: Text
    } deriving (Show)

data GttGetOrdersClass = GttGetOrdersClass
    { additionalPropertiesGttGetOrdersClass :: Bool
    , propertiesGttGetOrdersClass :: GttGetOrdersProperties
    , requiredGttGetOrdersClass :: Vector Text
    , titleGttGetOrdersClass :: Text
    , gttGetOrdersClassTypeGttGetOrdersClass :: Text
    } deriving (Show)

data GttGetOrdersProperties = GttGetOrdersProperties
    { gttGetOrdersPropertiesDataGttGetOrdersProperties :: Orders
    , statusGttGetOrdersProperties :: Exchange
    } deriving (Show)

data MetaClass = MetaClass
    { additionalPropertiesMetaClass :: Bool
    , titleMetaClass :: Text
    , metaClassTypeMetaClass :: Text
    } deriving (Show)

data Order = Order
    { additionalPropertiesOrder :: Bool
    , propertiesOrder :: OrderProperties
    , requiredOrder :: Vector Text
    , titleOrder :: Text
    , orderTypeOrder :: Text
    } deriving (Show)

data OrderProperties = OrderProperties
    { exchangeOrderProperties :: Exchange
    , orderTypeOrderProperties :: Exchange
    , priceOrderProperties :: Exchange
    , productOrderProperties :: Exchange
    , quantityOrderProperties :: Exchange
    , resultOrderProperties :: Meta
    , tradingsymbolOrderProperties :: Exchange
    , transactionTypeOrderProperties :: Exchange
    } deriving (Show)

data OrderResult = OrderResult
    { additionalPropertiesOrderResult :: Bool
    , propertiesOrderResult :: OrderResultProperties
    , requiredOrderResult :: Vector Text
    , titleOrderResult :: Text
    , orderResultTypeOrderResult :: Text
    } deriving (Show)

data OrderResultProperties = OrderResultProperties
    { orderIDOrderResultProperties :: Exchange
    , rejectionReasonOrderResultProperties :: Exchange
    , statusOrderResultProperties :: Exchange
    } deriving (Show)

data ResultClass = ResultClass
    { additionalPropertiesResultClass :: Bool
    , propertiesResultClass :: ResultProperties
    , requiredResultClass :: Vector Text
    , titleResultClass :: Text
    , resultTypeResultClass :: Text
    } deriving (Show)

data ResultProperties = ResultProperties
    { accountIDResultProperties :: Exchange
    , exchangeResultProperties :: Exchange
    , metaResultProperties :: Exchange
    , orderResultResultProperties :: ConditionClass
    , orderTypeResultProperties :: Exchange
    , priceResultProperties :: Exchange
    , productResultProperties :: Exchange
    , quantityResultProperties :: Exchange
    , timestampResultProperties :: CreatedAt
    , tradingsymbolResultProperties :: Exchange
    , transactionTypeResultProperties :: Exchange
    , triggeredAtResultProperties :: Exchange
    , validityResultProperties :: Exchange
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe GttGetOrders
decodeTopLevel = decode

instance ToJSON GttGetOrders where
    toJSON (GttGetOrders refGttGetOrders schemaGttGetOrders definitionsGttGetOrders) =
        object
        [ "$ref" .= refGttGetOrders
        , "$schema" .= schemaGttGetOrders
        , "definitions" .= definitionsGttGetOrders
        ]

instance FromJSON GttGetOrders where
    parseJSON (Object v) = GttGetOrders
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions conditionDefinitions datumDefinitions gttGetOrdersDefinitions metaDefinitions orderDefinitions orderResultDefinitions resultDefinitions) =
        object
        [ "Condition" .= conditionDefinitions
        , "Datum" .= datumDefinitions
        , "GttGetOrders" .= gttGetOrdersDefinitions
        , "Meta" .= metaDefinitions
        , "Order" .= orderDefinitions
        , "OrderResult" .= orderResultDefinitions
        , "Result" .= resultDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Condition"
        <*> v .: "Datum"
        <*> v .: "GttGetOrders"
        <*> v .: "Meta"
        <*> v .: "Order"
        <*> v .: "OrderResult"
        <*> v .: "Result"

instance ToJSON Condition where
    toJSON (Condition additionalPropertiesCondition propertiesCondition requiredCondition titleCondition conditionTypeCondition) =
        object
        [ "additionalProperties" .= additionalPropertiesCondition
        , "properties" .= propertiesCondition
        , "required" .= requiredCondition
        , "title" .= titleCondition
        , "type" .= conditionTypeCondition
        ]

instance FromJSON Condition where
    parseJSON (Object v) = Condition
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON ConditionProperties where
    toJSON (ConditionProperties exchangeConditionProperties instrumentTokenConditionProperties lastPriceConditionProperties tradingsymbolConditionProperties triggerValuesConditionProperties) =
        object
        [ "exchange" .= exchangeConditionProperties
        , "instrument_token" .= instrumentTokenConditionProperties
        , "last_price" .= lastPriceConditionProperties
        , "tradingsymbol" .= tradingsymbolConditionProperties
        , "trigger_values" .= triggerValuesConditionProperties
        ]

instance FromJSON ConditionProperties where
    parseJSON (Object v) = ConditionProperties
        <$> v .: "exchange"
        <*> v .: "instrument_token"
        <*> v .: "last_price"
        <*> v .: "tradingsymbol"
        <*> v .: "trigger_values"

instance ToJSON Exchange where
    toJSON (Exchange exchangeTypeExchange) =
        object
        [ "type" .= exchangeTypeExchange
        ]

instance FromJSON Exchange where
    parseJSON (Object v) = Exchange
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

instance ToJSON TriggerValues where
    toJSON (TriggerValues itemsTriggerValues triggerValuesTypeTriggerValues) =
        object
        [ "items" .= itemsTriggerValues
        , "type" .= triggerValuesTypeTriggerValues
        ]

instance FromJSON TriggerValues where
    parseJSON (Object v) = TriggerValues
        <$> v .: "items"
        <*> v .: "type"

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
    toJSON (DatumProperties conditionDatumProperties createdAtDatumProperties expiresAtDatumProperties datumPropertiesIDDatumProperties metaDatumProperties ordersDatumProperties parentTriggerDatumProperties statusDatumProperties datumPropertiesTypeDatumProperties updatedAtDatumProperties userIDDatumProperties) =
        object
        [ "condition" .= conditionDatumProperties
        , "created_at" .= createdAtDatumProperties
        , "expires_at" .= expiresAtDatumProperties
        , "id" .= datumPropertiesIDDatumProperties
        , "meta" .= metaDatumProperties
        , "orders" .= ordersDatumProperties
        , "parent_trigger" .= parentTriggerDatumProperties
        , "status" .= statusDatumProperties
        , "type" .= datumPropertiesTypeDatumProperties
        , "updated_at" .= updatedAtDatumProperties
        , "user_id" .= userIDDatumProperties
        ]

instance FromJSON DatumProperties where
    parseJSON (Object v) = DatumProperties
        <$> v .: "condition"
        <*> v .: "created_at"
        <*> v .: "expires_at"
        <*> v .: "id"
        <*> v .: "meta"
        <*> v .: "orders"
        <*> v .: "parent_trigger"
        <*> v .: "status"
        <*> v .: "type"
        <*> v .: "updated_at"
        <*> v .: "user_id"

instance ToJSON ConditionClass where
    toJSON (ConditionClass refConditionClass) =
        object
        [ "$ref" .= refConditionClass
        ]

instance FromJSON ConditionClass where
    parseJSON (Object v) = ConditionClass
        <$> v .: "$ref"

instance ToJSON CreatedAt where
    toJSON (CreatedAt formatCreatedAt createdAtTypeCreatedAt) =
        object
        [ "format" .= formatCreatedAt
        , "type" .= createdAtTypeCreatedAt
        ]

instance FromJSON CreatedAt where
    parseJSON (Object v) = CreatedAt
        <$> v .: "format"
        <*> v .: "type"

instance ToJSON Meta where
    toJSON (Meta anyOfMeta) =
        object
        [ "anyOf" .= anyOfMeta
        ]

instance FromJSON Meta where
    parseJSON (Object v) = Meta
        <$> v .: "anyOf"

instance ToJSON AnyOf where
    toJSON (AnyOf refAnyOf anyOfTypeAnyOf) =
        object
        [ "$ref" .= refAnyOf
        , "type" .= anyOfTypeAnyOf
        ]

instance FromJSON AnyOf where
    parseJSON (Object v) = AnyOf
        <$> v .:? "$ref"
        <*> v .:? "type"

instance ToJSON Orders where
    toJSON (Orders itemsOrders ordersTypeOrders) =
        object
        [ "items" .= itemsOrders
        , "type" .= ordersTypeOrders
        ]

instance FromJSON Orders where
    parseJSON (Object v) = Orders
        <$> v .: "items"
        <*> v .: "type"

instance ToJSON GttGetOrdersClass where
    toJSON (GttGetOrdersClass additionalPropertiesGttGetOrdersClass propertiesGttGetOrdersClass requiredGttGetOrdersClass titleGttGetOrdersClass gttGetOrdersClassTypeGttGetOrdersClass) =
        object
        [ "additionalProperties" .= additionalPropertiesGttGetOrdersClass
        , "properties" .= propertiesGttGetOrdersClass
        , "required" .= requiredGttGetOrdersClass
        , "title" .= titleGttGetOrdersClass
        , "type" .= gttGetOrdersClassTypeGttGetOrdersClass
        ]

instance FromJSON GttGetOrdersClass where
    parseJSON (Object v) = GttGetOrdersClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON GttGetOrdersProperties where
    toJSON (GttGetOrdersProperties gttGetOrdersPropertiesDataGttGetOrdersProperties statusGttGetOrdersProperties) =
        object
        [ "data" .= gttGetOrdersPropertiesDataGttGetOrdersProperties
        , "status" .= statusGttGetOrdersProperties
        ]

instance FromJSON GttGetOrdersProperties where
    parseJSON (Object v) = GttGetOrdersProperties
        <$> v .: "data"
        <*> v .: "status"

instance ToJSON MetaClass where
    toJSON (MetaClass additionalPropertiesMetaClass titleMetaClass metaClassTypeMetaClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMetaClass
        , "title" .= titleMetaClass
        , "type" .= metaClassTypeMetaClass
        ]

instance FromJSON MetaClass where
    parseJSON (Object v) = MetaClass
        <$> v .: "additionalProperties"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON Order where
    toJSON (Order additionalPropertiesOrder propertiesOrder requiredOrder titleOrder orderTypeOrder) =
        object
        [ "additionalProperties" .= additionalPropertiesOrder
        , "properties" .= propertiesOrder
        , "required" .= requiredOrder
        , "title" .= titleOrder
        , "type" .= orderTypeOrder
        ]

instance FromJSON Order where
    parseJSON (Object v) = Order
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON OrderProperties where
    toJSON (OrderProperties exchangeOrderProperties orderTypeOrderProperties priceOrderProperties productOrderProperties quantityOrderProperties resultOrderProperties tradingsymbolOrderProperties transactionTypeOrderProperties) =
        object
        [ "exchange" .= exchangeOrderProperties
        , "order_type" .= orderTypeOrderProperties
        , "price" .= priceOrderProperties
        , "product" .= productOrderProperties
        , "quantity" .= quantityOrderProperties
        , "result" .= resultOrderProperties
        , "tradingsymbol" .= tradingsymbolOrderProperties
        , "transaction_type" .= transactionTypeOrderProperties
        ]

instance FromJSON OrderProperties where
    parseJSON (Object v) = OrderProperties
        <$> v .: "exchange"
        <*> v .: "order_type"
        <*> v .: "price"
        <*> v .: "product"
        <*> v .: "quantity"
        <*> v .: "result"
        <*> v .: "tradingsymbol"
        <*> v .: "transaction_type"

instance ToJSON OrderResult where
    toJSON (OrderResult additionalPropertiesOrderResult propertiesOrderResult requiredOrderResult titleOrderResult orderResultTypeOrderResult) =
        object
        [ "additionalProperties" .= additionalPropertiesOrderResult
        , "properties" .= propertiesOrderResult
        , "required" .= requiredOrderResult
        , "title" .= titleOrderResult
        , "type" .= orderResultTypeOrderResult
        ]

instance FromJSON OrderResult where
    parseJSON (Object v) = OrderResult
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON OrderResultProperties where
    toJSON (OrderResultProperties orderIDOrderResultProperties rejectionReasonOrderResultProperties statusOrderResultProperties) =
        object
        [ "order_id" .= orderIDOrderResultProperties
        , "rejection_reason" .= rejectionReasonOrderResultProperties
        , "status" .= statusOrderResultProperties
        ]

instance FromJSON OrderResultProperties where
    parseJSON (Object v) = OrderResultProperties
        <$> v .: "order_id"
        <*> v .: "rejection_reason"
        <*> v .: "status"

instance ToJSON ResultClass where
    toJSON (ResultClass additionalPropertiesResultClass propertiesResultClass requiredResultClass titleResultClass resultTypeResultClass) =
        object
        [ "additionalProperties" .= additionalPropertiesResultClass
        , "properties" .= propertiesResultClass
        , "required" .= requiredResultClass
        , "title" .= titleResultClass
        , "type" .= resultTypeResultClass
        ]

instance FromJSON ResultClass where
    parseJSON (Object v) = ResultClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON ResultProperties where
    toJSON (ResultProperties accountIDResultProperties exchangeResultProperties metaResultProperties orderResultResultProperties orderTypeResultProperties priceResultProperties productResultProperties quantityResultProperties timestampResultProperties tradingsymbolResultProperties transactionTypeResultProperties triggeredAtResultProperties validityResultProperties) =
        object
        [ "account_id" .= accountIDResultProperties
        , "exchange" .= exchangeResultProperties
        , "meta" .= metaResultProperties
        , "order_result" .= orderResultResultProperties
        , "order_type" .= orderTypeResultProperties
        , "price" .= priceResultProperties
        , "product" .= productResultProperties
        , "quantity" .= quantityResultProperties
        , "timestamp" .= timestampResultProperties
        , "tradingsymbol" .= tradingsymbolResultProperties
        , "transaction_type" .= transactionTypeResultProperties
        , "triggered_at" .= triggeredAtResultProperties
        , "validity" .= validityResultProperties
        ]

instance FromJSON ResultProperties where
    parseJSON (Object v) = ResultProperties
        <$> v .: "account_id"
        <*> v .: "exchange"
        <*> v .: "meta"
        <*> v .: "order_result"
        <*> v .: "order_type"
        <*> v .: "price"
        <*> v .: "product"
        <*> v .: "quantity"
        <*> v .: "timestamp"
        <*> v .: "tradingsymbol"
        <*> v .: "transaction_type"
        <*> v .: "triggered_at"
        <*> v .: "validity"
