{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( GttGetOrder (..)
    , Definitions (..)
    , Condition (..)
    , ConditionProperties (..)
    , Exchange (..)
    , TriggerValues (..)
    , Data (..)
    , DataProperties (..)
    , ConditionClass (..)
    , CreatedAt (..)
    , Orders (..)
    , GttGetOrderClass (..)
    , GttGetOrderProperties (..)
    , Order (..)
    , OrderProperties (..)
    , ResultClass (..)
    , AnyOf (..)
    , OrderResult (..)
    , OrderResultProperties (..)
    , DefinitionsResult (..)
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

data GttGetOrder = GttGetOrder
    { refGttGetOrder :: Text
    , schemaGttGetOrder :: Text
    , definitionsGttGetOrder :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { conditionDefinitions :: Condition
    , definitionsDataDefinitions :: Data
    , gttGetOrderDefinitions :: GttGetOrderClass
    , orderDefinitions :: Order
    , orderResultDefinitions :: OrderResult
    , resultDefinitions :: DefinitionsResult
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

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { conditionDataProperties :: ConditionClass
    , createdAtDataProperties :: CreatedAt
    , expiresAtDataProperties :: CreatedAt
    , dataPropertiesIDDataProperties :: Exchange
    , metaDataProperties :: Exchange
    , ordersDataProperties :: Orders
    , parentTriggerDataProperties :: Exchange
    , statusDataProperties :: Exchange
    , dataPropertiesTypeDataProperties :: Exchange
    , updatedAtDataProperties :: CreatedAt
    , userIDDataProperties :: Exchange
    } deriving (Show)

data ConditionClass = ConditionClass
    { refConditionClass :: Text
    } deriving (Show)

data CreatedAt = CreatedAt
    { formatCreatedAt :: Text
    , createdAtTypeCreatedAt :: Type
    } deriving (Show)

data Orders = Orders
    { itemsOrders :: ConditionClass
    , ordersTypeOrders :: Text
    } deriving (Show)

data GttGetOrderClass = GttGetOrderClass
    { additionalPropertiesGttGetOrderClass :: Bool
    , propertiesGttGetOrderClass :: GttGetOrderProperties
    , requiredGttGetOrderClass :: Vector Text
    , titleGttGetOrderClass :: Text
    , gttGetOrderClassTypeGttGetOrderClass :: Text
    } deriving (Show)

data GttGetOrderProperties = GttGetOrderProperties
    { gttGetOrderPropertiesDataGttGetOrderProperties :: ConditionClass
    , statusGttGetOrderProperties :: Exchange
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
    , resultOrderProperties :: ResultClass
    , tradingsymbolOrderProperties :: Exchange
    , transactionTypeOrderProperties :: Exchange
    } deriving (Show)

data ResultClass = ResultClass
    { anyOfResultClass :: Vector AnyOf
    } deriving (Show)

data AnyOf = AnyOf
    { refAnyOf :: Maybe Text
    , anyOfTypeAnyOf :: Maybe Type
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

data DefinitionsResult = DefinitionsResult
    { additionalPropertiesDefinitionsResult :: Bool
    , propertiesDefinitionsResult :: ResultProperties
    , requiredDefinitionsResult :: Vector Text
    , titleDefinitionsResult :: Text
    , resultClassTypeDefinitionsResult :: Text
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

decodeTopLevel :: ByteString -> Maybe GttGetOrder
decodeTopLevel = decode

instance ToJSON GttGetOrder where
    toJSON (GttGetOrder refGttGetOrder schemaGttGetOrder definitionsGttGetOrder) =
        object
        [ "$ref" .= refGttGetOrder
        , "$schema" .= schemaGttGetOrder
        , "definitions" .= definitionsGttGetOrder
        ]

instance FromJSON GttGetOrder where
    parseJSON (Object v) = GttGetOrder
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions conditionDefinitions definitionsDataDefinitions gttGetOrderDefinitions orderDefinitions orderResultDefinitions resultDefinitions) =
        object
        [ "Condition" .= conditionDefinitions
        , "Data" .= definitionsDataDefinitions
        , "GttGetOrder" .= gttGetOrderDefinitions
        , "Order" .= orderDefinitions
        , "OrderResult" .= orderResultDefinitions
        , "Result" .= resultDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Condition"
        <*> v .: "Data"
        <*> v .: "GttGetOrder"
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
    toJSON (DataProperties conditionDataProperties createdAtDataProperties expiresAtDataProperties dataPropertiesIDDataProperties metaDataProperties ordersDataProperties parentTriggerDataProperties statusDataProperties dataPropertiesTypeDataProperties updatedAtDataProperties userIDDataProperties) =
        object
        [ "condition" .= conditionDataProperties
        , "created_at" .= createdAtDataProperties
        , "expires_at" .= expiresAtDataProperties
        , "id" .= dataPropertiesIDDataProperties
        , "meta" .= metaDataProperties
        , "orders" .= ordersDataProperties
        , "parent_trigger" .= parentTriggerDataProperties
        , "status" .= statusDataProperties
        , "type" .= dataPropertiesTypeDataProperties
        , "updated_at" .= updatedAtDataProperties
        , "user_id" .= userIDDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
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

instance ToJSON GttGetOrderClass where
    toJSON (GttGetOrderClass additionalPropertiesGttGetOrderClass propertiesGttGetOrderClass requiredGttGetOrderClass titleGttGetOrderClass gttGetOrderClassTypeGttGetOrderClass) =
        object
        [ "additionalProperties" .= additionalPropertiesGttGetOrderClass
        , "properties" .= propertiesGttGetOrderClass
        , "required" .= requiredGttGetOrderClass
        , "title" .= titleGttGetOrderClass
        , "type" .= gttGetOrderClassTypeGttGetOrderClass
        ]

instance FromJSON GttGetOrderClass where
    parseJSON (Object v) = GttGetOrderClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON GttGetOrderProperties where
    toJSON (GttGetOrderProperties gttGetOrderPropertiesDataGttGetOrderProperties statusGttGetOrderProperties) =
        object
        [ "data" .= gttGetOrderPropertiesDataGttGetOrderProperties
        , "status" .= statusGttGetOrderProperties
        ]

instance FromJSON GttGetOrderProperties where
    parseJSON (Object v) = GttGetOrderProperties
        <$> v .: "data"
        <*> v .: "status"

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

instance ToJSON ResultClass where
    toJSON (ResultClass anyOfResultClass) =
        object
        [ "anyOf" .= anyOfResultClass
        ]

instance FromJSON ResultClass where
    parseJSON (Object v) = ResultClass
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

instance ToJSON DefinitionsResult where
    toJSON (DefinitionsResult additionalPropertiesDefinitionsResult propertiesDefinitionsResult requiredDefinitionsResult titleDefinitionsResult resultClassTypeDefinitionsResult) =
        object
        [ "additionalProperties" .= additionalPropertiesDefinitionsResult
        , "properties" .= propertiesDefinitionsResult
        , "required" .= requiredDefinitionsResult
        , "title" .= titleDefinitionsResult
        , "type" .= resultClassTypeDefinitionsResult
        ]

instance FromJSON DefinitionsResult where
    parseJSON (Object v) = DefinitionsResult
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
