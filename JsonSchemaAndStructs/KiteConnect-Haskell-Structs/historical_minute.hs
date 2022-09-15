{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( HistoricalMinute (..)
    , Definitions (..)
    , Candle (..)
    , AnyOf (..)
    , Data (..)
    , DataProperties (..)
    , Candles (..)
    , Items (..)
    , DataClass (..)
    , HistoricalMinuteClass (..)
    , HistoricalMinuteProperties (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data HistoricalMinute = HistoricalMinute
    { refHistoricalMinute :: Text
    , schemaHistoricalMinute :: Text
    , definitionsHistoricalMinute :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { candleDefinitions :: Candle
    , definitionsDataDefinitions :: Data
    , historicalMinuteDefinitions :: HistoricalMinuteClass
    } deriving (Show)

data Candle = Candle
    { anyOfCandle :: Vector AnyOf
    , titleCandle :: Text
    } deriving (Show)

data AnyOf = AnyOf
    { anyOfTypeAnyOf :: Text
    } deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { candlesDataProperties :: Candles
    } deriving (Show)

data Candles = Candles
    { itemsCandles :: Items
    , candlesTypeCandles :: Text
    } deriving (Show)

data Items = Items
    { itemsItems :: DataClass
    , itemsTypeItems :: Text
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

data HistoricalMinuteClass = HistoricalMinuteClass
    { additionalPropertiesHistoricalMinuteClass :: Bool
    , propertiesHistoricalMinuteClass :: HistoricalMinuteProperties
    , requiredHistoricalMinuteClass :: Vector Text
    , titleHistoricalMinuteClass :: Text
    , historicalMinuteClassTypeHistoricalMinuteClass :: Text
    } deriving (Show)

data HistoricalMinuteProperties = HistoricalMinuteProperties
    { historicalMinutePropertiesDataHistoricalMinuteProperties :: DataClass
    , statusHistoricalMinuteProperties :: AnyOf
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe HistoricalMinute
decodeTopLevel = decode

instance ToJSON HistoricalMinute where
    toJSON (HistoricalMinute refHistoricalMinute schemaHistoricalMinute definitionsHistoricalMinute) =
        object
        [ "$ref" .= refHistoricalMinute
        , "$schema" .= schemaHistoricalMinute
        , "definitions" .= definitionsHistoricalMinute
        ]

instance FromJSON HistoricalMinute where
    parseJSON (Object v) = HistoricalMinute
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions candleDefinitions definitionsDataDefinitions historicalMinuteDefinitions) =
        object
        [ "Candle" .= candleDefinitions
        , "Data" .= definitionsDataDefinitions
        , "HistoricalMinute" .= historicalMinuteDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Candle"
        <*> v .: "Data"
        <*> v .: "HistoricalMinute"

instance ToJSON Candle where
    toJSON (Candle anyOfCandle titleCandle) =
        object
        [ "anyOf" .= anyOfCandle
        , "title" .= titleCandle
        ]

instance FromJSON Candle where
    parseJSON (Object v) = Candle
        <$> v .: "anyOf"
        <*> v .: "title"

instance ToJSON AnyOf where
    toJSON (AnyOf anyOfTypeAnyOf) =
        object
        [ "type" .= anyOfTypeAnyOf
        ]

instance FromJSON AnyOf where
    parseJSON (Object v) = AnyOf
        <$> v .: "type"

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
    toJSON (DataProperties candlesDataProperties) =
        object
        [ "candles" .= candlesDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "candles"

instance ToJSON Candles where
    toJSON (Candles itemsCandles candlesTypeCandles) =
        object
        [ "items" .= itemsCandles
        , "type" .= candlesTypeCandles
        ]

instance FromJSON Candles where
    parseJSON (Object v) = Candles
        <$> v .: "items"
        <*> v .: "type"

instance ToJSON Items where
    toJSON (Items itemsItems itemsTypeItems) =
        object
        [ "items" .= itemsItems
        , "type" .= itemsTypeItems
        ]

instance FromJSON Items where
    parseJSON (Object v) = Items
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

instance ToJSON HistoricalMinuteClass where
    toJSON (HistoricalMinuteClass additionalPropertiesHistoricalMinuteClass propertiesHistoricalMinuteClass requiredHistoricalMinuteClass titleHistoricalMinuteClass historicalMinuteClassTypeHistoricalMinuteClass) =
        object
        [ "additionalProperties" .= additionalPropertiesHistoricalMinuteClass
        , "properties" .= propertiesHistoricalMinuteClass
        , "required" .= requiredHistoricalMinuteClass
        , "title" .= titleHistoricalMinuteClass
        , "type" .= historicalMinuteClassTypeHistoricalMinuteClass
        ]

instance FromJSON HistoricalMinuteClass where
    parseJSON (Object v) = HistoricalMinuteClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON HistoricalMinuteProperties where
    toJSON (HistoricalMinuteProperties historicalMinutePropertiesDataHistoricalMinuteProperties statusHistoricalMinuteProperties) =
        object
        [ "data" .= historicalMinutePropertiesDataHistoricalMinuteProperties
        , "status" .= statusHistoricalMinuteProperties
        ]

instance FromJSON HistoricalMinuteProperties where
    parseJSON (Object v) = HistoricalMinuteProperties
        <$> v .: "data"
        <*> v .: "status"
