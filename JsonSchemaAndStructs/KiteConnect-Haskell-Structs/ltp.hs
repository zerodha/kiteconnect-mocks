{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( Ltp (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , NseInfy (..)
    , LtpClass (..)
    , LtpProperties (..)
    , Status (..)
    , NseInfyClass (..)
    , NseInfyProperties (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Ltp = Ltp
    { refLtp :: Text
    , schemaLtp :: Text
    , definitionsLtp :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , ltpDefinitions :: LtpClass
    , nseInfyDefinitions :: NseInfyClass
    } deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { nseInfyDataProperties :: NseInfy
    } deriving (Show)

data NseInfy = NseInfy
    { refNseInfy :: Text
    } deriving (Show)

data LtpClass = LtpClass
    { additionalPropertiesLtpClass :: Bool
    , propertiesLtpClass :: LtpProperties
    , requiredLtpClass :: Vector Text
    , titleLtpClass :: Text
    , ltpClassTypeLtpClass :: Text
    } deriving (Show)

data LtpProperties = LtpProperties
    { ltpPropertiesDataLtpProperties :: NseInfy
    , statusLtpProperties :: Status
    } deriving (Show)

data Status = Status
    { statusTypeStatus :: Text
    } deriving (Show)

data NseInfyClass = NseInfyClass
    { additionalPropertiesNseInfyClass :: Bool
    , propertiesNseInfyClass :: NseInfyProperties
    , requiredNseInfyClass :: Vector Text
    , titleNseInfyClass :: Text
    , nseInfyClassTypeNseInfyClass :: Text
    } deriving (Show)

data NseInfyProperties = NseInfyProperties
    { instrumentTokenNseInfyProperties :: Status
    , lastPriceNseInfyProperties :: Status
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Ltp
decodeTopLevel = decode

instance ToJSON Ltp where
    toJSON (Ltp refLtp schemaLtp definitionsLtp) =
        object
        [ "$ref" .= refLtp
        , "$schema" .= schemaLtp
        , "definitions" .= definitionsLtp
        ]

instance FromJSON Ltp where
    parseJSON (Object v) = Ltp
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions ltpDefinitions nseInfyDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "Ltp" .= ltpDefinitions
        , "NseInfy" .= nseInfyDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "Ltp"
        <*> v .: "NseInfy"

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
    toJSON (DataProperties nseInfyDataProperties) =
        object
        [ "NSE:INFY" .= nseInfyDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "NSE:INFY"

instance ToJSON NseInfy where
    toJSON (NseInfy refNseInfy) =
        object
        [ "$ref" .= refNseInfy
        ]

instance FromJSON NseInfy where
    parseJSON (Object v) = NseInfy
        <$> v .: "$ref"

instance ToJSON LtpClass where
    toJSON (LtpClass additionalPropertiesLtpClass propertiesLtpClass requiredLtpClass titleLtpClass ltpClassTypeLtpClass) =
        object
        [ "additionalProperties" .= additionalPropertiesLtpClass
        , "properties" .= propertiesLtpClass
        , "required" .= requiredLtpClass
        , "title" .= titleLtpClass
        , "type" .= ltpClassTypeLtpClass
        ]

instance FromJSON LtpClass where
    parseJSON (Object v) = LtpClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON LtpProperties where
    toJSON (LtpProperties ltpPropertiesDataLtpProperties statusLtpProperties) =
        object
        [ "data" .= ltpPropertiesDataLtpProperties
        , "status" .= statusLtpProperties
        ]

instance FromJSON LtpProperties where
    parseJSON (Object v) = LtpProperties
        <$> v .: "data"
        <*> v .: "status"

instance ToJSON Status where
    toJSON (Status statusTypeStatus) =
        object
        [ "type" .= statusTypeStatus
        ]

instance FromJSON Status where
    parseJSON (Object v) = Status
        <$> v .: "type"

instance ToJSON NseInfyClass where
    toJSON (NseInfyClass additionalPropertiesNseInfyClass propertiesNseInfyClass requiredNseInfyClass titleNseInfyClass nseInfyClassTypeNseInfyClass) =
        object
        [ "additionalProperties" .= additionalPropertiesNseInfyClass
        , "properties" .= propertiesNseInfyClass
        , "required" .= requiredNseInfyClass
        , "title" .= titleNseInfyClass
        , "type" .= nseInfyClassTypeNseInfyClass
        ]

instance FromJSON NseInfyClass where
    parseJSON (Object v) = NseInfyClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON NseInfyProperties where
    toJSON (NseInfyProperties instrumentTokenNseInfyProperties lastPriceNseInfyProperties) =
        object
        [ "instrument_token" .= instrumentTokenNseInfyProperties
        , "last_price" .= lastPriceNseInfyProperties
        ]

instance FromJSON NseInfyProperties where
    parseJSON (Object v) = NseInfyProperties
        <$> v .: "instrument_token"
        <*> v .: "last_price"
