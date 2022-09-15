{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( MFSIPPlace (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , Sipid (..)
    , MFSIPPlaceClass (..)
    , MFSIPPlaceProperties (..)
    , DataClass (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MFSIPPlace = MFSIPPlace
    { refMFSIPPlace :: Text
    , schemaMFSIPPlace :: Text
    , definitionsMFSIPPlace :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , mfsipPlaceDefinitions :: MFSIPPlaceClass
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

data MFSIPPlaceClass = MFSIPPlaceClass
    { additionalPropertiesMFSIPPlaceClass :: Bool
    , propertiesMFSIPPlaceClass :: MFSIPPlaceProperties
    , requiredMFSIPPlaceClass :: Vector Text
    , titleMFSIPPlaceClass :: Text
    , mfsipPlaceClassTypeMFSIPPlaceClass :: Text
    } deriving (Show)

data MFSIPPlaceProperties = MFSIPPlaceProperties
    { mfsipPlacePropertiesDataMFSIPPlaceProperties :: DataClass
    , statusMFSIPPlaceProperties :: Sipid
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MFSIPPlace
decodeTopLevel = decode

instance ToJSON MFSIPPlace where
    toJSON (MFSIPPlace refMFSIPPlace schemaMFSIPPlace definitionsMFSIPPlace) =
        object
        [ "$ref" .= refMFSIPPlace
        , "$schema" .= schemaMFSIPPlace
        , "definitions" .= definitionsMFSIPPlace
        ]

instance FromJSON MFSIPPlace where
    parseJSON (Object v) = MFSIPPlace
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions mfsipPlaceDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "MFSIPPlace" .= mfsipPlaceDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "MFSIPPlace"

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

instance ToJSON MFSIPPlaceClass where
    toJSON (MFSIPPlaceClass additionalPropertiesMFSIPPlaceClass propertiesMFSIPPlaceClass requiredMFSIPPlaceClass titleMFSIPPlaceClass mfsipPlaceClassTypeMFSIPPlaceClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMFSIPPlaceClass
        , "properties" .= propertiesMFSIPPlaceClass
        , "required" .= requiredMFSIPPlaceClass
        , "title" .= titleMFSIPPlaceClass
        , "type" .= mfsipPlaceClassTypeMFSIPPlaceClass
        ]

instance FromJSON MFSIPPlaceClass where
    parseJSON (Object v) = MFSIPPlaceClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MFSIPPlaceProperties where
    toJSON (MFSIPPlaceProperties mfsipPlacePropertiesDataMFSIPPlaceProperties statusMFSIPPlaceProperties) =
        object
        [ "data" .= mfsipPlacePropertiesDataMFSIPPlaceProperties
        , "status" .= statusMFSIPPlaceProperties
        ]

instance FromJSON MFSIPPlaceProperties where
    parseJSON (Object v) = MFSIPPlaceProperties
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
