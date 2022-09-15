{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( MFOrderCancel (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , OrderID (..)
    , MFOrderCancelClass (..)
    , MFOrderCancelProperties (..)
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

data MFOrderCancel = MFOrderCancel
    { refMFOrderCancel :: Text
    , schemaMFOrderCancel :: Text
    , definitionsMFOrderCancel :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , mfOrderCancelDefinitions :: MFOrderCancelClass
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

data MFOrderCancelClass = MFOrderCancelClass
    { additionalPropertiesMFOrderCancelClass :: Bool
    , propertiesMFOrderCancelClass :: MFOrderCancelProperties
    , requiredMFOrderCancelClass :: Vector Text
    , titleMFOrderCancelClass :: Text
    , mfOrderCancelClassTypeMFOrderCancelClass :: Text
    } deriving (Show)

data MFOrderCancelProperties = MFOrderCancelProperties
    { mfOrderCancelPropertiesDataMFOrderCancelProperties :: DataClass
    , statusMFOrderCancelProperties :: Status
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

data Status = Status
    { statusTypeStatus :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFOrderCancel
decodeTopLevel = decode

instance ToJSON MFOrderCancel where
    toJSON (MFOrderCancel refMFOrderCancel schemaMFOrderCancel definitionsMFOrderCancel) =
        object
        [ "$ref" .= refMFOrderCancel
        , "$schema" .= schemaMFOrderCancel
        , "definitions" .= definitionsMFOrderCancel
        ]

instance FromJSON MFOrderCancel where
    parseJSON (Object v) = MFOrderCancel
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions mfOrderCancelDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "MFOrderCancel" .= mfOrderCancelDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "MFOrderCancel"

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

instance ToJSON MFOrderCancelClass where
    toJSON (MFOrderCancelClass additionalPropertiesMFOrderCancelClass propertiesMFOrderCancelClass requiredMFOrderCancelClass titleMFOrderCancelClass mfOrderCancelClassTypeMFOrderCancelClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMFOrderCancelClass
        , "properties" .= propertiesMFOrderCancelClass
        , "required" .= requiredMFOrderCancelClass
        , "title" .= titleMFOrderCancelClass
        , "type" .= mfOrderCancelClassTypeMFOrderCancelClass
        ]

instance FromJSON MFOrderCancelClass where
    parseJSON (Object v) = MFOrderCancelClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MFOrderCancelProperties where
    toJSON (MFOrderCancelProperties mfOrderCancelPropertiesDataMFOrderCancelProperties statusMFOrderCancelProperties) =
        object
        [ "data" .= mfOrderCancelPropertiesDataMFOrderCancelProperties
        , "status" .= statusMFOrderCancelProperties
        ]

instance FromJSON MFOrderCancelProperties where
    parseJSON (Object v) = MFOrderCancelProperties
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
