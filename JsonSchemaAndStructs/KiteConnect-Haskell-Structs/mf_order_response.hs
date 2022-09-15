{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( MFOrderResponse (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , OrderID (..)
    , MFOrderResponseClass (..)
    , MFOrderResponseProperties (..)
    , DataClass (..)
    , Status (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFOrderResponse = MFOrderResponse
    { refMFOrderResponse :: Text
    , schemaMFOrderResponse :: Text
    , definitionsMFOrderResponse :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , mfOrderResponseDefinitions :: MFOrderResponseClass
    } deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { orderIDDataProperties :: OrderID
    } deriving (Show)

data OrderID = OrderID
    { formatOrderID :: Text
    , orderIDTypeOrderID :: Text
    } deriving (Show)

data MFOrderResponseClass = MFOrderResponseClass
    { additionalPropertiesMFOrderResponseClass :: Bool
    , propertiesMFOrderResponseClass :: MFOrderResponseProperties
    , requiredMFOrderResponseClass :: Vector Text
    , titleMFOrderResponseClass :: Text
    , mfOrderResponseClassTypeMFOrderResponseClass :: Text
    } deriving (Show)

data MFOrderResponseProperties = MFOrderResponseProperties
    { mfOrderResponsePropertiesDataMFOrderResponseProperties :: DataClass
    , statusMFOrderResponseProperties :: Status
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

data Status = Status
    { statusTypeStatus :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFOrderResponse
decodeTopLevel = decode

instance ToJSON MFOrderResponse where
    toJSON (MFOrderResponse refMFOrderResponse schemaMFOrderResponse definitionsMFOrderResponse) =
        object
        [ "$ref" .= refMFOrderResponse
        , "$schema" .= schemaMFOrderResponse
        , "definitions" .= definitionsMFOrderResponse
        ]

instance FromJSON MFOrderResponse where
    parseJSON (Object v) = MFOrderResponse
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions mfOrderResponseDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "MFOrderResponse" .= mfOrderResponseDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "MFOrderResponse"

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
    toJSON (DataProperties orderIDDataProperties) =
        object
        [ "order_id" .= orderIDDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "order_id"

instance ToJSON OrderID where
    toJSON (OrderID formatOrderID orderIDTypeOrderID) =
        object
        [ "format" .= formatOrderID
        , "type" .= orderIDTypeOrderID
        ]

instance FromJSON OrderID where
    parseJSON (Object v) = OrderID
        <$> v .: "format"
        <*> v .: "type"

instance ToJSON MFOrderResponseClass where
    toJSON (MFOrderResponseClass additionalPropertiesMFOrderResponseClass propertiesMFOrderResponseClass requiredMFOrderResponseClass titleMFOrderResponseClass mfOrderResponseClassTypeMFOrderResponseClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMFOrderResponseClass
        , "properties" .= propertiesMFOrderResponseClass
        , "required" .= requiredMFOrderResponseClass
        , "title" .= titleMFOrderResponseClass
        , "type" .= mfOrderResponseClassTypeMFOrderResponseClass
        ]

instance FromJSON MFOrderResponseClass where
    parseJSON (Object v) = MFOrderResponseClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MFOrderResponseProperties where
    toJSON (MFOrderResponseProperties mfOrderResponsePropertiesDataMFOrderResponseProperties statusMFOrderResponseProperties) =
        object
        [ "data" .= mfOrderResponsePropertiesDataMFOrderResponseProperties
        , "status" .= statusMFOrderResponseProperties
        ]

instance FromJSON MFOrderResponseProperties where
    parseJSON (Object v) = MFOrderResponseProperties
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

instance ToJSON Status where
    toJSON (Status statusTypeStatus) =
        object
        [ "type" .= statusTypeStatus
        ]

instance FromJSON Status where
    parseJSON (Object v) = Status
        <$> v .: "type"
