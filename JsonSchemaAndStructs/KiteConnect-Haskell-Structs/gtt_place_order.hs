{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( GttPlaceOrder (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , TriggerID (..)
    , GttPlaceOrderClass (..)
    , GttPlaceOrderProperties (..)
    , DataClass (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data GttPlaceOrder = GttPlaceOrder
    { refGttPlaceOrder :: Text
    , schemaGttPlaceOrder :: Text
    , definitionsGttPlaceOrder :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , gttPlaceOrderDefinitions :: GttPlaceOrderClass
    } deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { triggerIDDataProperties :: TriggerID
    } deriving (Show)

data TriggerID = TriggerID
    { triggerIDTypeTriggerID :: Text
    } deriving (Show)

data GttPlaceOrderClass = GttPlaceOrderClass
    { additionalPropertiesGttPlaceOrderClass :: Bool
    , propertiesGttPlaceOrderClass :: GttPlaceOrderProperties
    , requiredGttPlaceOrderClass :: Vector Text
    , titleGttPlaceOrderClass :: Text
    , gttPlaceOrderClassTypeGttPlaceOrderClass :: Text
    } deriving (Show)

data GttPlaceOrderProperties = GttPlaceOrderProperties
    { gttPlaceOrderPropertiesDataGttPlaceOrderProperties :: DataClass
    , statusGttPlaceOrderProperties :: TriggerID
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe GttPlaceOrder
decodeTopLevel = decode

instance ToJSON GttPlaceOrder where
    toJSON (GttPlaceOrder refGttPlaceOrder schemaGttPlaceOrder definitionsGttPlaceOrder) =
        object
        [ "$ref" .= refGttPlaceOrder
        , "$schema" .= schemaGttPlaceOrder
        , "definitions" .= definitionsGttPlaceOrder
        ]

instance FromJSON GttPlaceOrder where
    parseJSON (Object v) = GttPlaceOrder
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions gttPlaceOrderDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "GttPlaceOrder" .= gttPlaceOrderDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "GttPlaceOrder"

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
    toJSON (DataProperties triggerIDDataProperties) =
        object
        [ "trigger_id" .= triggerIDDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "trigger_id"

instance ToJSON TriggerID where
    toJSON (TriggerID triggerIDTypeTriggerID) =
        object
        [ "type" .= triggerIDTypeTriggerID
        ]

instance FromJSON TriggerID where
    parseJSON (Object v) = TriggerID
        <$> v .: "type"

instance ToJSON GttPlaceOrderClass where
    toJSON (GttPlaceOrderClass additionalPropertiesGttPlaceOrderClass propertiesGttPlaceOrderClass requiredGttPlaceOrderClass titleGttPlaceOrderClass gttPlaceOrderClassTypeGttPlaceOrderClass) =
        object
        [ "additionalProperties" .= additionalPropertiesGttPlaceOrderClass
        , "properties" .= propertiesGttPlaceOrderClass
        , "required" .= requiredGttPlaceOrderClass
        , "title" .= titleGttPlaceOrderClass
        , "type" .= gttPlaceOrderClassTypeGttPlaceOrderClass
        ]

instance FromJSON GttPlaceOrderClass where
    parseJSON (Object v) = GttPlaceOrderClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON GttPlaceOrderProperties where
    toJSON (GttPlaceOrderProperties gttPlaceOrderPropertiesDataGttPlaceOrderProperties statusGttPlaceOrderProperties) =
        object
        [ "data" .= gttPlaceOrderPropertiesDataGttPlaceOrderProperties
        , "status" .= statusGttPlaceOrderProperties
        ]

instance FromJSON GttPlaceOrderProperties where
    parseJSON (Object v) = GttPlaceOrderProperties
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
