{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( GttDeleteOrder (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , TriggerID (..)
    , GttDeleteOrderClass (..)
    , GttDeleteOrderProperties (..)
    , DataClass (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data GttDeleteOrder = GttDeleteOrder
    { refGttDeleteOrder :: Text
    , schemaGttDeleteOrder :: Text
    , definitionsGttDeleteOrder :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , gttDeleteOrderDefinitions :: GttDeleteOrderClass
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

data GttDeleteOrderClass = GttDeleteOrderClass
    { additionalPropertiesGttDeleteOrderClass :: Bool
    , propertiesGttDeleteOrderClass :: GttDeleteOrderProperties
    , requiredGttDeleteOrderClass :: Vector Text
    , titleGttDeleteOrderClass :: Text
    , gttDeleteOrderClassTypeGttDeleteOrderClass :: Text
    } deriving (Show)

data GttDeleteOrderProperties = GttDeleteOrderProperties
    { gttDeleteOrderPropertiesDataGttDeleteOrderProperties :: DataClass
    , statusGttDeleteOrderProperties :: TriggerID
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe GttDeleteOrder
decodeTopLevel = decode

instance ToJSON GttDeleteOrder where
    toJSON (GttDeleteOrder refGttDeleteOrder schemaGttDeleteOrder definitionsGttDeleteOrder) =
        object
        [ "$ref" .= refGttDeleteOrder
        , "$schema" .= schemaGttDeleteOrder
        , "definitions" .= definitionsGttDeleteOrder
        ]

instance FromJSON GttDeleteOrder where
    parseJSON (Object v) = GttDeleteOrder
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions gttDeleteOrderDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "GttDeleteOrder" .= gttDeleteOrderDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "GttDeleteOrder"

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

instance ToJSON GttDeleteOrderClass where
    toJSON (GttDeleteOrderClass additionalPropertiesGttDeleteOrderClass propertiesGttDeleteOrderClass requiredGttDeleteOrderClass titleGttDeleteOrderClass gttDeleteOrderClassTypeGttDeleteOrderClass) =
        object
        [ "additionalProperties" .= additionalPropertiesGttDeleteOrderClass
        , "properties" .= propertiesGttDeleteOrderClass
        , "required" .= requiredGttDeleteOrderClass
        , "title" .= titleGttDeleteOrderClass
        , "type" .= gttDeleteOrderClassTypeGttDeleteOrderClass
        ]

instance FromJSON GttDeleteOrderClass where
    parseJSON (Object v) = GttDeleteOrderClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON GttDeleteOrderProperties where
    toJSON (GttDeleteOrderProperties gttDeleteOrderPropertiesDataGttDeleteOrderProperties statusGttDeleteOrderProperties) =
        object
        [ "data" .= gttDeleteOrderPropertiesDataGttDeleteOrderProperties
        , "status" .= statusGttDeleteOrderProperties
        ]

instance FromJSON GttDeleteOrderProperties where
    parseJSON (Object v) = GttDeleteOrderProperties
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
