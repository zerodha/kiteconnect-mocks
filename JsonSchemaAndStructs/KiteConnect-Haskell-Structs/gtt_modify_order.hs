{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( GttModifyOrder (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , TriggerID (..)
    , GttModifyOrderClass (..)
    , GttModifyOrderProperties (..)
    , DataClass (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data GttModifyOrder = GttModifyOrder
    { refGttModifyOrder :: Text
    , schemaGttModifyOrder :: Text
    , definitionsGttModifyOrder :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , gttModifyOrderDefinitions :: GttModifyOrderClass
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

data GttModifyOrderClass = GttModifyOrderClass
    { additionalPropertiesGttModifyOrderClass :: Bool
    , propertiesGttModifyOrderClass :: GttModifyOrderProperties
    , requiredGttModifyOrderClass :: Vector Text
    , titleGttModifyOrderClass :: Text
    , gttModifyOrderClassTypeGttModifyOrderClass :: Text
    } deriving (Show)

data GttModifyOrderProperties = GttModifyOrderProperties
    { gttModifyOrderPropertiesDataGttModifyOrderProperties :: DataClass
    , statusGttModifyOrderProperties :: TriggerID
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe GttModifyOrder
decodeTopLevel = decode

instance ToJSON GttModifyOrder where
    toJSON (GttModifyOrder refGttModifyOrder schemaGttModifyOrder definitionsGttModifyOrder) =
        object
        [ "$ref" .= refGttModifyOrder
        , "$schema" .= schemaGttModifyOrder
        , "definitions" .= definitionsGttModifyOrder
        ]

instance FromJSON GttModifyOrder where
    parseJSON (Object v) = GttModifyOrder
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions gttModifyOrderDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "GttModifyOrder" .= gttModifyOrderDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "GttModifyOrder"

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

instance ToJSON GttModifyOrderClass where
    toJSON (GttModifyOrderClass additionalPropertiesGttModifyOrderClass propertiesGttModifyOrderClass requiredGttModifyOrderClass titleGttModifyOrderClass gttModifyOrderClassTypeGttModifyOrderClass) =
        object
        [ "additionalProperties" .= additionalPropertiesGttModifyOrderClass
        , "properties" .= propertiesGttModifyOrderClass
        , "required" .= requiredGttModifyOrderClass
        , "title" .= titleGttModifyOrderClass
        , "type" .= gttModifyOrderClassTypeGttModifyOrderClass
        ]

instance FromJSON GttModifyOrderClass where
    parseJSON (Object v) = GttModifyOrderClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON GttModifyOrderProperties where
    toJSON (GttModifyOrderProperties gttModifyOrderPropertiesDataGttModifyOrderProperties statusGttModifyOrderProperties) =
        object
        [ "data" .= gttModifyOrderPropertiesDataGttModifyOrderProperties
        , "status" .= statusGttModifyOrderProperties
        ]

instance FromJSON GttModifyOrderProperties where
    parseJSON (Object v) = GttModifyOrderProperties
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
