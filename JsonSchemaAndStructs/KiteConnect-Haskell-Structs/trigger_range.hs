{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( TriggerRange (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , NseInfy (..)
    , Nse (..)
    , NseProperties (..)
    , InstrumentToken (..)
    , TriggerRangeClass (..)
    , TriggerRangeProperties (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data TriggerRange = TriggerRange
    { refTriggerRange :: Text
    , schemaTriggerRange :: Text
    , definitionsTriggerRange :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , nseDefinitions :: Nse
    , triggerRangeDefinitions :: TriggerRangeClass
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
    , nseRelianceDataProperties :: NseInfy
    } deriving (Show)

data NseInfy = NseInfy
    { refNseInfy :: Text
    } deriving (Show)

data Nse = Nse
    { additionalPropertiesNse :: Bool
    , propertiesNse :: NseProperties
    , requiredNse :: Vector Text
    , titleNse :: Text
    , nseTypeNse :: Text
    } deriving (Show)

data NseProperties = NseProperties
    { instrumentTokenNseProperties :: InstrumentToken
    , lowerNseProperties :: InstrumentToken
    , upperNseProperties :: InstrumentToken
    } deriving (Show)

data InstrumentToken = InstrumentToken
    { instrumentTokenTypeInstrumentToken :: Text
    } deriving (Show)

data TriggerRangeClass = TriggerRangeClass
    { additionalPropertiesTriggerRangeClass :: Bool
    , propertiesTriggerRangeClass :: TriggerRangeProperties
    , requiredTriggerRangeClass :: Vector Text
    , titleTriggerRangeClass :: Text
    , triggerRangeClassTypeTriggerRangeClass :: Text
    } deriving (Show)

data TriggerRangeProperties = TriggerRangeProperties
    { triggerRangePropertiesDataTriggerRangeProperties :: NseInfy
    , statusTriggerRangeProperties :: InstrumentToken
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe TriggerRange
decodeTopLevel = decode

instance ToJSON TriggerRange where
    toJSON (TriggerRange refTriggerRange schemaTriggerRange definitionsTriggerRange) =
        object
        [ "$ref" .= refTriggerRange
        , "$schema" .= schemaTriggerRange
        , "definitions" .= definitionsTriggerRange
        ]

instance FromJSON TriggerRange where
    parseJSON (Object v) = TriggerRange
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions nseDefinitions triggerRangeDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "Nse" .= nseDefinitions
        , "TriggerRange" .= triggerRangeDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "Nse"
        <*> v .: "TriggerRange"

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
    toJSON (DataProperties nseInfyDataProperties nseRelianceDataProperties) =
        object
        [ "NSE:INFY" .= nseInfyDataProperties
        , "NSE:RELIANCE" .= nseRelianceDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "NSE:INFY"
        <*> v .: "NSE:RELIANCE"

instance ToJSON NseInfy where
    toJSON (NseInfy refNseInfy) =
        object
        [ "$ref" .= refNseInfy
        ]

instance FromJSON NseInfy where
    parseJSON (Object v) = NseInfy
        <$> v .: "$ref"

instance ToJSON Nse where
    toJSON (Nse additionalPropertiesNse propertiesNse requiredNse titleNse nseTypeNse) =
        object
        [ "additionalProperties" .= additionalPropertiesNse
        , "properties" .= propertiesNse
        , "required" .= requiredNse
        , "title" .= titleNse
        , "type" .= nseTypeNse
        ]

instance FromJSON Nse where
    parseJSON (Object v) = Nse
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON NseProperties where
    toJSON (NseProperties instrumentTokenNseProperties lowerNseProperties upperNseProperties) =
        object
        [ "instrument_token" .= instrumentTokenNseProperties
        , "lower" .= lowerNseProperties
        , "upper" .= upperNseProperties
        ]

instance FromJSON NseProperties where
    parseJSON (Object v) = NseProperties
        <$> v .: "instrument_token"
        <*> v .: "lower"
        <*> v .: "upper"

instance ToJSON InstrumentToken where
    toJSON (InstrumentToken instrumentTokenTypeInstrumentToken) =
        object
        [ "type" .= instrumentTokenTypeInstrumentToken
        ]

instance FromJSON InstrumentToken where
    parseJSON (Object v) = InstrumentToken
        <$> v .: "type"

instance ToJSON TriggerRangeClass where
    toJSON (TriggerRangeClass additionalPropertiesTriggerRangeClass propertiesTriggerRangeClass requiredTriggerRangeClass titleTriggerRangeClass triggerRangeClassTypeTriggerRangeClass) =
        object
        [ "additionalProperties" .= additionalPropertiesTriggerRangeClass
        , "properties" .= propertiesTriggerRangeClass
        , "required" .= requiredTriggerRangeClass
        , "title" .= titleTriggerRangeClass
        , "type" .= triggerRangeClassTypeTriggerRangeClass
        ]

instance FromJSON TriggerRangeClass where
    parseJSON (Object v) = TriggerRangeClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON TriggerRangeProperties where
    toJSON (TriggerRangeProperties triggerRangePropertiesDataTriggerRangeProperties statusTriggerRangeProperties) =
        object
        [ "data" .= triggerRangePropertiesDataTriggerRangeProperties
        , "status" .= statusTriggerRangeProperties
        ]

instance FromJSON TriggerRangeProperties where
    parseJSON (Object v) = TriggerRangeProperties
        <$> v .: "data"
        <*> v .: "status"
