{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( ConvertPosition (..)
    , Definitions (..)
    , ConvertPositionClass (..)
    , Properties (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data ConvertPosition = ConvertPosition
    { refConvertPosition :: Text
    , schemaConvertPosition :: Text
    , definitionsConvertPosition :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { convertPositionDefinitions :: ConvertPositionClass
    } deriving (Show)

data ConvertPositionClass = ConvertPositionClass
    { additionalPropertiesConvertPositionClass :: Bool
    , propertiesConvertPositionClass :: Properties
    , requiredConvertPositionClass :: Vector Text
    , titleConvertPositionClass :: Text
    , convertPositionClassTypeConvertPositionClass :: Text
    } deriving (Show)

data Properties = Properties
    { propertiesDataProperties :: Data
    , statusProperties :: Data
    } deriving (Show)

data Data = Data
    { dataTypeData :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe ConvertPosition
decodeTopLevel = decode

instance ToJSON ConvertPosition where
    toJSON (ConvertPosition refConvertPosition schemaConvertPosition definitionsConvertPosition) =
        object
        [ "$ref" .= refConvertPosition
        , "$schema" .= schemaConvertPosition
        , "definitions" .= definitionsConvertPosition
        ]

instance FromJSON ConvertPosition where
    parseJSON (Object v) = ConvertPosition
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions convertPositionDefinitions) =
        object
        [ "ConvertPosition" .= convertPositionDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "ConvertPosition"

instance ToJSON ConvertPositionClass where
    toJSON (ConvertPositionClass additionalPropertiesConvertPositionClass propertiesConvertPositionClass requiredConvertPositionClass titleConvertPositionClass convertPositionClassTypeConvertPositionClass) =
        object
        [ "additionalProperties" .= additionalPropertiesConvertPositionClass
        , "properties" .= propertiesConvertPositionClass
        , "required" .= requiredConvertPositionClass
        , "title" .= titleConvertPositionClass
        , "type" .= convertPositionClassTypeConvertPositionClass
        ]

instance FromJSON ConvertPositionClass where
    parseJSON (Object v) = ConvertPositionClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON Properties where
    toJSON (Properties propertiesDataProperties statusProperties) =
        object
        [ "data" .= propertiesDataProperties
        , "status" .= statusProperties
        ]

instance FromJSON Properties where
    parseJSON (Object v) = Properties
        <$> v .: "data"
        <*> v .: "status"

instance ToJSON Data where
    toJSON (Data dataTypeData) =
        object
        [ "type" .= dataTypeData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "type"
