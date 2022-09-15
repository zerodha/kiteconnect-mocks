{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( Postback (..)
    , Definitions (..)
    , Meta (..)
    , PostbackClass (..)
    , Properties (..)
    , AppID (..)
    , Timestamp (..)
    , MetaClass (..)
    , Type (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Postback = Postback
    { refPostback :: Text
    , schemaPostback :: Text
    , definitionsPostback :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { metaDefinitions :: Meta
    , postbackDefinitions :: PostbackClass
    } deriving (Show)

data Meta = Meta
    { additionalPropertiesMeta :: Bool
    , titleMeta :: Text
    , metaTypeMeta :: Text
    } deriving (Show)

data PostbackClass = PostbackClass
    { additionalPropertiesPostbackClass :: Bool
    , propertiesPostbackClass :: Properties
    , requiredPostbackClass :: Vector Text
    , titlePostbackClass :: Text
    , postbackClassTypePostbackClass :: Text
    } deriving (Show)

data Properties = Properties
    { appIDProperties :: AppID
    , averagePriceProperties :: AppID
    , cancelledQuantityProperties :: AppID
    , checksumProperties :: AppID
    , disclosedQuantityProperties :: AppID
    , exchangeProperties :: AppID
    , exchangeOrderIDProperties :: AppID
    , exchangeTimestampProperties :: Timestamp
    , exchangeUpdateTimestampProperties :: Timestamp
    , filledQuantityProperties :: AppID
    , guidProperties :: AppID
    , instrumentTokenProperties :: AppID
    , marketProtectionProperties :: AppID
    , metaProperties :: MetaClass
    , orderIDProperties :: AppID
    , orderTimestampProperties :: Timestamp
    , orderTypeProperties :: AppID
    , parentOrderIDProperties :: AppID
    , pendingQuantityProperties :: AppID
    , placedByProperties :: AppID
    , priceProperties :: AppID
    , productProperties :: AppID
    , quantityProperties :: AppID
    , statusProperties :: AppID
    , statusMessageProperties :: AppID
    , statusMessageRawProperties :: AppID
    , tagProperties :: AppID
    , tradingsymbolProperties :: AppID
    , transactionTypeProperties :: AppID
    , triggerPriceProperties :: AppID
    , unfilledQuantityProperties :: AppID
    , userIDProperties :: AppID
    , validityProperties :: AppID
    , varietyProperties :: AppID
    } deriving (Show)

data AppID = AppID
    { appIDTypeAppID :: Type
    } deriving (Show)

data Type
    = IntegerType
    | NullType
    | StringType
    deriving (Show)

data Timestamp = Timestamp
    { formatTimestamp :: Text
    , timestampTypeTimestamp :: Type
    } deriving (Show)

data MetaClass = MetaClass
    { refMetaClass :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Postback
decodeTopLevel = decode

instance ToJSON Postback where
    toJSON (Postback refPostback schemaPostback definitionsPostback) =
        object
        [ "$ref" .= refPostback
        , "$schema" .= schemaPostback
        , "definitions" .= definitionsPostback
        ]

instance FromJSON Postback where
    parseJSON (Object v) = Postback
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions metaDefinitions postbackDefinitions) =
        object
        [ "Meta" .= metaDefinitions
        , "Postback" .= postbackDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Meta"
        <*> v .: "Postback"

instance ToJSON Meta where
    toJSON (Meta additionalPropertiesMeta titleMeta metaTypeMeta) =
        object
        [ "additionalProperties" .= additionalPropertiesMeta
        , "title" .= titleMeta
        , "type" .= metaTypeMeta
        ]

instance FromJSON Meta where
    parseJSON (Object v) = Meta
        <$> v .: "additionalProperties"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON PostbackClass where
    toJSON (PostbackClass additionalPropertiesPostbackClass propertiesPostbackClass requiredPostbackClass titlePostbackClass postbackClassTypePostbackClass) =
        object
        [ "additionalProperties" .= additionalPropertiesPostbackClass
        , "properties" .= propertiesPostbackClass
        , "required" .= requiredPostbackClass
        , "title" .= titlePostbackClass
        , "type" .= postbackClassTypePostbackClass
        ]

instance FromJSON PostbackClass where
    parseJSON (Object v) = PostbackClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON Properties where
    toJSON (Properties appIDProperties averagePriceProperties cancelledQuantityProperties checksumProperties disclosedQuantityProperties exchangeProperties exchangeOrderIDProperties exchangeTimestampProperties exchangeUpdateTimestampProperties filledQuantityProperties guidProperties instrumentTokenProperties marketProtectionProperties metaProperties orderIDProperties orderTimestampProperties orderTypeProperties parentOrderIDProperties pendingQuantityProperties placedByProperties priceProperties productProperties quantityProperties statusProperties statusMessageProperties statusMessageRawProperties tagProperties tradingsymbolProperties transactionTypeProperties triggerPriceProperties unfilledQuantityProperties userIDProperties validityProperties varietyProperties) =
        object
        [ "app_id" .= appIDProperties
        , "average_price" .= averagePriceProperties
        , "cancelled_quantity" .= cancelledQuantityProperties
        , "checksum" .= checksumProperties
        , "disclosed_quantity" .= disclosedQuantityProperties
        , "exchange" .= exchangeProperties
        , "exchange_order_id" .= exchangeOrderIDProperties
        , "exchange_timestamp" .= exchangeTimestampProperties
        , "exchange_update_timestamp" .= exchangeUpdateTimestampProperties
        , "filled_quantity" .= filledQuantityProperties
        , "guid" .= guidProperties
        , "instrument_token" .= instrumentTokenProperties
        , "market_protection" .= marketProtectionProperties
        , "meta" .= metaProperties
        , "order_id" .= orderIDProperties
        , "order_timestamp" .= orderTimestampProperties
        , "order_type" .= orderTypeProperties
        , "parent_order_id" .= parentOrderIDProperties
        , "pending_quantity" .= pendingQuantityProperties
        , "placed_by" .= placedByProperties
        , "price" .= priceProperties
        , "product" .= productProperties
        , "quantity" .= quantityProperties
        , "status" .= statusProperties
        , "status_message" .= statusMessageProperties
        , "status_message_raw" .= statusMessageRawProperties
        , "tag" .= tagProperties
        , "tradingsymbol" .= tradingsymbolProperties
        , "transaction_type" .= transactionTypeProperties
        , "trigger_price" .= triggerPriceProperties
        , "unfilled_quantity" .= unfilledQuantityProperties
        , "user_id" .= userIDProperties
        , "validity" .= validityProperties
        , "variety" .= varietyProperties
        ]

instance FromJSON Properties where
    parseJSON (Object v) = Properties
        <$> v .: "app_id"
        <*> v .: "average_price"
        <*> v .: "cancelled_quantity"
        <*> v .: "checksum"
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
        <*> v .: "tradingsymbol"
        <*> v .: "transaction_type"
        <*> v .: "trigger_price"
        <*> v .: "unfilled_quantity"
        <*> v .: "user_id"
        <*> v .: "validity"
        <*> v .: "variety"

instance ToJSON AppID where
    toJSON (AppID appIDTypeAppID) =
        object
        [ "type" .= appIDTypeAppID
        ]

instance FromJSON AppID where
    parseJSON (Object v) = AppID
        <$> v .: "type"

instance ToJSON Type where
    toJSON IntegerType = "integer"
    toJSON NullType = "null"
    toJSON StringType = "string"

instance FromJSON Type where
    parseJSON = withText "Type" parseText
        where
            parseText "integer" = return IntegerType
            parseText "null" = return NullType
            parseText "string" = return StringType

instance ToJSON Timestamp where
    toJSON (Timestamp formatTimestamp timestampTypeTimestamp) =
        object
        [ "format" .= formatTimestamp
        , "type" .= timestampTypeTimestamp
        ]

instance FromJSON Timestamp where
    parseJSON (Object v) = Timestamp
        <$> v .: "format"
        <*> v .: "type"

instance ToJSON MetaClass where
    toJSON (MetaClass refMetaClass) =
        object
        [ "$ref" .= refMetaClass
        ]

instance FromJSON MetaClass where
    parseJSON (Object v) = MetaClass
        <$> v .: "$ref"
