{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( Ohlc (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , NseInfy (..)
    , NseInfyClass (..)
    , NseInfyProperties (..)
    , InstrumentToken (..)
    , OhlcClass (..)
    , OhlcProperties (..)
    , OhlcClassClass (..)
    , OhlcClassProperties (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Ohlc = Ohlc
    { refOhlc :: Text
    , schemaOhlc :: Text
    , definitionsOhlc :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , nseInfyDefinitions :: NseInfyClass
    , ohlcDefinitions :: OhlcClass
    , ohlcClassDefinitions :: OhlcClassClass
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

data NseInfyClass = NseInfyClass
    { additionalPropertiesNseInfyClass :: Bool
    , propertiesNseInfyClass :: NseInfyProperties
    , requiredNseInfyClass :: Vector Text
    , titleNseInfyClass :: Text
    , nseInfyClassTypeNseInfyClass :: Text
    } deriving (Show)

data NseInfyProperties = NseInfyProperties
    { instrumentTokenNseInfyProperties :: InstrumentToken
    , lastPriceNseInfyProperties :: InstrumentToken
    , ohlcNseInfyProperties :: NseInfy
    } deriving (Show)

data InstrumentToken = InstrumentToken
    { instrumentTokenTypeInstrumentToken :: Text
    } deriving (Show)

data OhlcClass = OhlcClass
    { additionalPropertiesOhlcClass :: Bool
    , propertiesOhlcClass :: OhlcProperties
    , requiredOhlcClass :: Vector Text
    , titleOhlcClass :: Text
    , ohlcClassTypeOhlcClass :: Text
    } deriving (Show)

data OhlcProperties = OhlcProperties
    { ohlcPropertiesDataOhlcProperties :: NseInfy
    , statusOhlcProperties :: InstrumentToken
    } deriving (Show)

data OhlcClassClass = OhlcClassClass
    { additionalPropertiesOhlcClassClass :: Bool
    , propertiesOhlcClassClass :: OhlcClassProperties
    , requiredOhlcClassClass :: Vector Text
    , titleOhlcClassClass :: Text
    , ohlcClassClassTypeOhlcClassClass :: Text
    } deriving (Show)

data OhlcClassProperties = OhlcClassProperties
    { closeOhlcClassProperties :: InstrumentToken
    , highOhlcClassProperties :: InstrumentToken
    , lowOhlcClassProperties :: InstrumentToken
    , openOhlcClassProperties :: InstrumentToken
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Ohlc
decodeTopLevel = decode

instance ToJSON Ohlc where
    toJSON (Ohlc refOhlc schemaOhlc definitionsOhlc) =
        object
        [ "$ref" .= refOhlc
        , "$schema" .= schemaOhlc
        , "definitions" .= definitionsOhlc
        ]

instance FromJSON Ohlc where
    parseJSON (Object v) = Ohlc
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions nseInfyDefinitions ohlcDefinitions ohlcClassDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "NseInfy" .= nseInfyDefinitions
        , "Ohlc" .= ohlcDefinitions
        , "OhlcClass" .= ohlcClassDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "NseInfy"
        <*> v .: "Ohlc"
        <*> v .: "OhlcClass"

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
    toJSON (NseInfyProperties instrumentTokenNseInfyProperties lastPriceNseInfyProperties ohlcNseInfyProperties) =
        object
        [ "instrument_token" .= instrumentTokenNseInfyProperties
        , "last_price" .= lastPriceNseInfyProperties
        , "ohlc" .= ohlcNseInfyProperties
        ]

instance FromJSON NseInfyProperties where
    parseJSON (Object v) = NseInfyProperties
        <$> v .: "instrument_token"
        <*> v .: "last_price"
        <*> v .: "ohlc"

instance ToJSON InstrumentToken where
    toJSON (InstrumentToken instrumentTokenTypeInstrumentToken) =
        object
        [ "type" .= instrumentTokenTypeInstrumentToken
        ]

instance FromJSON InstrumentToken where
    parseJSON (Object v) = InstrumentToken
        <$> v .: "type"

instance ToJSON OhlcClass where
    toJSON (OhlcClass additionalPropertiesOhlcClass propertiesOhlcClass requiredOhlcClass titleOhlcClass ohlcClassTypeOhlcClass) =
        object
        [ "additionalProperties" .= additionalPropertiesOhlcClass
        , "properties" .= propertiesOhlcClass
        , "required" .= requiredOhlcClass
        , "title" .= titleOhlcClass
        , "type" .= ohlcClassTypeOhlcClass
        ]

instance FromJSON OhlcClass where
    parseJSON (Object v) = OhlcClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON OhlcProperties where
    toJSON (OhlcProperties ohlcPropertiesDataOhlcProperties statusOhlcProperties) =
        object
        [ "data" .= ohlcPropertiesDataOhlcProperties
        , "status" .= statusOhlcProperties
        ]

instance FromJSON OhlcProperties where
    parseJSON (Object v) = OhlcProperties
        <$> v .: "data"
        <*> v .: "status"

instance ToJSON OhlcClassClass where
    toJSON (OhlcClassClass additionalPropertiesOhlcClassClass propertiesOhlcClassClass requiredOhlcClassClass titleOhlcClassClass ohlcClassClassTypeOhlcClassClass) =
        object
        [ "additionalProperties" .= additionalPropertiesOhlcClassClass
        , "properties" .= propertiesOhlcClassClass
        , "required" .= requiredOhlcClassClass
        , "title" .= titleOhlcClassClass
        , "type" .= ohlcClassClassTypeOhlcClassClass
        ]

instance FromJSON OhlcClassClass where
    parseJSON (Object v) = OhlcClassClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON OhlcClassProperties where
    toJSON (OhlcClassProperties closeOhlcClassProperties highOhlcClassProperties lowOhlcClassProperties openOhlcClassProperties) =
        object
        [ "close" .= closeOhlcClassProperties
        , "high" .= highOhlcClassProperties
        , "low" .= lowOhlcClassProperties
        , "open" .= openOhlcClassProperties
        ]

instance FromJSON OhlcClassProperties where
    parseJSON (Object v) = OhlcClassProperties
        <$> v .: "close"
        <*> v .: "high"
        <*> v .: "low"
        <*> v .: "open"
