{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( MFSIPCancel (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , Sipid (..)
    , MFSIPCancelClass (..)
    , MFSIPCancelProperties (..)
    , DataClass (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFSIPCancel = MFSIPCancel
    { refMFSIPCancel :: Text
    , schemaMFSIPCancel :: Text
    , definitionsMFSIPCancel :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , mfsipCancelDefinitions :: MFSIPCancelClass
    } deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { sipIDDataProperties :: Sipid
    } deriving (Show)

data Sipid = Sipid
    { sipidTypeSipid :: Text
    } deriving (Show)

data MFSIPCancelClass = MFSIPCancelClass
    { additionalPropertiesMFSIPCancelClass :: Bool
    , propertiesMFSIPCancelClass :: MFSIPCancelProperties
    , requiredMFSIPCancelClass :: Vector Text
    , titleMFSIPCancelClass :: Text
    , mfsipCancelClassTypeMFSIPCancelClass :: Text
    } deriving (Show)

data MFSIPCancelProperties = MFSIPCancelProperties
    { mfsipCancelPropertiesDataMFSIPCancelProperties :: DataClass
    , statusMFSIPCancelProperties :: Sipid
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFSIPCancel
decodeTopLevel = decode

instance ToJSON MFSIPCancel where
    toJSON (MFSIPCancel refMFSIPCancel schemaMFSIPCancel definitionsMFSIPCancel) =
        object
        [ "$ref" .= refMFSIPCancel
        , "$schema" .= schemaMFSIPCancel
        , "definitions" .= definitionsMFSIPCancel
        ]

instance FromJSON MFSIPCancel where
    parseJSON (Object v) = MFSIPCancel
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions mfsipCancelDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "MFSIPCancel" .= mfsipCancelDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "MFSIPCancel"

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
    toJSON (DataProperties sipIDDataProperties) =
        object
        [ "sip_id" .= sipIDDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "sip_id"

instance ToJSON Sipid where
    toJSON (Sipid sipidTypeSipid) =
        object
        [ "type" .= sipidTypeSipid
        ]

instance FromJSON Sipid where
    parseJSON (Object v) = Sipid
        <$> v .: "type"

instance ToJSON MFSIPCancelClass where
    toJSON (MFSIPCancelClass additionalPropertiesMFSIPCancelClass propertiesMFSIPCancelClass requiredMFSIPCancelClass titleMFSIPCancelClass mfsipCancelClassTypeMFSIPCancelClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMFSIPCancelClass
        , "properties" .= propertiesMFSIPCancelClass
        , "required" .= requiredMFSIPCancelClass
        , "title" .= titleMFSIPCancelClass
        , "type" .= mfsipCancelClassTypeMFSIPCancelClass
        ]

instance FromJSON MFSIPCancelClass where
    parseJSON (Object v) = MFSIPCancelClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MFSIPCancelProperties where
    toJSON (MFSIPCancelProperties mfsipCancelPropertiesDataMFSIPCancelProperties statusMFSIPCancelProperties) =
        object
        [ "data" .= mfsipCancelPropertiesDataMFSIPCancelProperties
        , "status" .= statusMFSIPCancelProperties
        ]

instance FromJSON MFSIPCancelProperties where
    parseJSON (Object v) = MFSIPCancelProperties
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
