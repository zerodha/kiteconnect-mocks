{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( HoldingsAuth (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , RequestID (..)
    , HoldingsAuthClass (..)
    , HoldingsAuthProperties (..)
    , DataClass (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data HoldingsAuth = HoldingsAuth
    { refHoldingsAuth :: Text
    , schemaHoldingsAuth :: Text
    , definitionsHoldingsAuth :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , holdingsAuthDefinitions :: HoldingsAuthClass
    } deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { requestIDDataProperties :: RequestID
    } deriving (Show)

data RequestID = RequestID
    { requestIDTypeRequestID :: Text
    } deriving (Show)

data HoldingsAuthClass = HoldingsAuthClass
    { additionalPropertiesHoldingsAuthClass :: Bool
    , propertiesHoldingsAuthClass :: HoldingsAuthProperties
    , requiredHoldingsAuthClass :: Vector Text
    , titleHoldingsAuthClass :: Text
    , holdingsAuthClassTypeHoldingsAuthClass :: Text
    } deriving (Show)

data HoldingsAuthProperties = HoldingsAuthProperties
    { holdingsAuthPropertiesDataHoldingsAuthProperties :: DataClass
    , statusHoldingsAuthProperties :: RequestID
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe HoldingsAuth
decodeTopLevel = decode

instance ToJSON HoldingsAuth where
    toJSON (HoldingsAuth refHoldingsAuth schemaHoldingsAuth definitionsHoldingsAuth) =
        object
        [ "$ref" .= refHoldingsAuth
        , "$schema" .= schemaHoldingsAuth
        , "definitions" .= definitionsHoldingsAuth
        ]

instance FromJSON HoldingsAuth where
    parseJSON (Object v) = HoldingsAuth
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions holdingsAuthDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "HoldingsAuth" .= holdingsAuthDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "HoldingsAuth"

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
    toJSON (DataProperties requestIDDataProperties) =
        object
        [ "request_id" .= requestIDDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "request_id"

instance ToJSON RequestID where
    toJSON (RequestID requestIDTypeRequestID) =
        object
        [ "type" .= requestIDTypeRequestID
        ]

instance FromJSON RequestID where
    parseJSON (Object v) = RequestID
        <$> v .: "type"

instance ToJSON HoldingsAuthClass where
    toJSON (HoldingsAuthClass additionalPropertiesHoldingsAuthClass propertiesHoldingsAuthClass requiredHoldingsAuthClass titleHoldingsAuthClass holdingsAuthClassTypeHoldingsAuthClass) =
        object
        [ "additionalProperties" .= additionalPropertiesHoldingsAuthClass
        , "properties" .= propertiesHoldingsAuthClass
        , "required" .= requiredHoldingsAuthClass
        , "title" .= titleHoldingsAuthClass
        , "type" .= holdingsAuthClassTypeHoldingsAuthClass
        ]

instance FromJSON HoldingsAuthClass where
    parseJSON (Object v) = HoldingsAuthClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON HoldingsAuthProperties where
    toJSON (HoldingsAuthProperties holdingsAuthPropertiesDataHoldingsAuthProperties statusHoldingsAuthProperties) =
        object
        [ "data" .= holdingsAuthPropertiesDataHoldingsAuthProperties
        , "status" .= statusHoldingsAuthProperties
        ]

instance FromJSON HoldingsAuthProperties where
    parseJSON (Object v) = HoldingsAuthProperties
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
