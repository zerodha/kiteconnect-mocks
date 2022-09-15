{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( Positions (..)
    , Definitions (..)
    , DayClass (..)
    , Property (..)
    , Data (..)
    , DataProperties (..)
    , Day (..)
    , DataClass (..)
    , PositionsClass (..)
    , PositionsProperties (..)
    , Type (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Positions = Positions
    { refPositions :: Text
    , schemaPositions :: Text
    , definitionsPositions :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , dayDefinitions :: DayClass
    , positionsDefinitions :: PositionsClass
    } deriving (Show)

data DayClass = DayClass
    { additionalPropertiesDayClass :: Bool
    , propertiesDayClass :: HashMap Text Property
    , requiredDayClass :: Vector Text
    , titleDayClass :: Text
    , dayClassTypeDayClass :: Text
    } deriving (Show)

data Property = Property
    { propertyTypeProperty :: Type
    } deriving (Show)

data Type
    = IntegerType
    | NumberType
    | StringType
    deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { dayDataProperties :: Day
    , netDataProperties :: Day
    } deriving (Show)

data Day = Day
    { itemsDay :: DataClass
    , dayTypeDay :: Text
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

data PositionsClass = PositionsClass
    { additionalPropertiesPositionsClass :: Bool
    , propertiesPositionsClass :: PositionsProperties
    , requiredPositionsClass :: Vector Text
    , titlePositionsClass :: Text
    , positionsClassTypePositionsClass :: Text
    } deriving (Show)

data PositionsProperties = PositionsProperties
    { positionsPropertiesDataPositionsProperties :: DataClass
    , statusPositionsProperties :: Property
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Positions
decodeTopLevel = decode

instance ToJSON Positions where
    toJSON (Positions refPositions schemaPositions definitionsPositions) =
        object
        [ "$ref" .= refPositions
        , "$schema" .= schemaPositions
        , "definitions" .= definitionsPositions
        ]

instance FromJSON Positions where
    parseJSON (Object v) = Positions
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions dayDefinitions positionsDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "Day" .= dayDefinitions
        , "Positions" .= positionsDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "Day"
        <*> v .: "Positions"

instance ToJSON DayClass where
    toJSON (DayClass additionalPropertiesDayClass propertiesDayClass requiredDayClass titleDayClass dayClassTypeDayClass) =
        object
        [ "additionalProperties" .= additionalPropertiesDayClass
        , "properties" .= propertiesDayClass
        , "required" .= requiredDayClass
        , "title" .= titleDayClass
        , "type" .= dayClassTypeDayClass
        ]

instance FromJSON DayClass where
    parseJSON (Object v) = DayClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON Property where
    toJSON (Property propertyTypeProperty) =
        object
        [ "type" .= propertyTypeProperty
        ]

instance FromJSON Property where
    parseJSON (Object v) = Property
        <$> v .: "type"

instance ToJSON Type where
    toJSON IntegerType = "integer"
    toJSON NumberType = "number"
    toJSON StringType = "string"

instance FromJSON Type where
    parseJSON = withText "Type" parseText
        where
            parseText "integer" = return IntegerType
            parseText "number" = return NumberType
            parseText "string" = return StringType

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
    toJSON (DataProperties dayDataProperties netDataProperties) =
        object
        [ "day" .= dayDataProperties
        , "net" .= netDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "day"
        <*> v .: "net"

instance ToJSON Day where
    toJSON (Day itemsDay dayTypeDay) =
        object
        [ "items" .= itemsDay
        , "type" .= dayTypeDay
        ]

instance FromJSON Day where
    parseJSON (Object v) = Day
        <$> v .: "items"
        <*> v .: "type"

instance ToJSON DataClass where
    toJSON (DataClass refDataClass) =
        object
        [ "$ref" .= refDataClass
        ]

instance FromJSON DataClass where
    parseJSON (Object v) = DataClass
        <$> v .: "$ref"

instance ToJSON PositionsClass where
    toJSON (PositionsClass additionalPropertiesPositionsClass propertiesPositionsClass requiredPositionsClass titlePositionsClass positionsClassTypePositionsClass) =
        object
        [ "additionalProperties" .= additionalPropertiesPositionsClass
        , "properties" .= propertiesPositionsClass
        , "required" .= requiredPositionsClass
        , "title" .= titlePositionsClass
        , "type" .= positionsClassTypePositionsClass
        ]

instance FromJSON PositionsClass where
    parseJSON (Object v) = PositionsClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON PositionsProperties where
    toJSON (PositionsProperties positionsPropertiesDataPositionsProperties statusPositionsProperties) =
        object
        [ "data" .= positionsPropertiesDataPositionsProperties
        , "status" .= statusPositionsProperties
        ]

instance FromJSON PositionsProperties where
    parseJSON (Object v) = PositionsProperties
        <$> v .: "data"
        <*> v .: "status"
