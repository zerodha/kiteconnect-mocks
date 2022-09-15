{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( MFSIPModify (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , Sipid (..)
    , MFSIPModifyClass (..)
    , MFSIPModifyProperties (..)
    , DataClass (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFSIPModify = MFSIPModify
    { refMFSIPModify :: Text
    , schemaMFSIPModify :: Text
    , definitionsMFSIPModify :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , mfsipModifyDefinitions :: MFSIPModifyClass
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

data MFSIPModifyClass = MFSIPModifyClass
    { additionalPropertiesMFSIPModifyClass :: Bool
    , propertiesMFSIPModifyClass :: MFSIPModifyProperties
    , requiredMFSIPModifyClass :: Vector Text
    , titleMFSIPModifyClass :: Text
    , mfsipModifyClassTypeMFSIPModifyClass :: Text
    } deriving (Show)

data MFSIPModifyProperties = MFSIPModifyProperties
    { mfsipModifyPropertiesDataMFSIPModifyProperties :: DataClass
    , statusMFSIPModifyProperties :: Sipid
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFSIPModify
decodeTopLevel = decode

instance ToJSON MFSIPModify where
    toJSON (MFSIPModify refMFSIPModify schemaMFSIPModify definitionsMFSIPModify) =
        object
        [ "$ref" .= refMFSIPModify
        , "$schema" .= schemaMFSIPModify
        , "definitions" .= definitionsMFSIPModify
        ]

instance FromJSON MFSIPModify where
    parseJSON (Object v) = MFSIPModify
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions mfsipModifyDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "MFSIPModify" .= mfsipModifyDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "MFSIPModify"

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

instance ToJSON MFSIPModifyClass where
    toJSON (MFSIPModifyClass additionalPropertiesMFSIPModifyClass propertiesMFSIPModifyClass requiredMFSIPModifyClass titleMFSIPModifyClass mfsipModifyClassTypeMFSIPModifyClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMFSIPModifyClass
        , "properties" .= propertiesMFSIPModifyClass
        , "required" .= requiredMFSIPModifyClass
        , "title" .= titleMFSIPModifyClass
        , "type" .= mfsipModifyClassTypeMFSIPModifyClass
        ]

instance FromJSON MFSIPModifyClass where
    parseJSON (Object v) = MFSIPModifyClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MFSIPModifyProperties where
    toJSON (MFSIPModifyProperties mfsipModifyPropertiesDataMFSIPModifyProperties statusMFSIPModifyProperties) =
        object
        [ "data" .= mfsipModifyPropertiesDataMFSIPModifyProperties
        , "status" .= statusMFSIPModifyProperties
        ]

instance FromJSON MFSIPModifyProperties where
    parseJSON (Object v) = MFSIPModifyProperties
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
