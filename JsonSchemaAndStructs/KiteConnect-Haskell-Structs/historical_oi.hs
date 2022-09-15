{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( HistoricalOi (..)
    , Definitions (..)
    , Candle (..)
    , AnyOf (..)
    , Data (..)
    , DataProperties (..)
    , Candles (..)
    , Items (..)
    , DataClass (..)
    , HistoricalOiClass (..)
    , HistoricalOiProperties (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data HistoricalOi = HistoricalOi
    { refHistoricalOi :: Text
    , schemaHistoricalOi :: Text
    , definitionsHistoricalOi :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { candleDefinitions :: Candle
    , definitionsDataDefinitions :: Data
    , historicalOiDefinitions :: HistoricalOiClass
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

data HistoricalOiClass = HistoricalOiClass
    { additionalPropertiesHistoricalOiClass :: Bool
    , propertiesHistoricalOiClass :: HistoricalOiProperties
    , requiredHistoricalOiClass :: Vector Text
    , titleHistoricalOiClass :: Text
    , historicalOiClassTypeHistoricalOiClass :: Text
    } deriving (Show)

data HistoricalOiProperties = HistoricalOiProperties
    { historicalOiPropertiesDataHistoricalOiProperties :: DataClass
    , statusHistoricalOiProperties :: AnyOf
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe HistoricalOi
decodeTopLevel = decode

instance ToJSON HistoricalOi where
    toJSON (HistoricalOi refHistoricalOi schemaHistoricalOi definitionsHistoricalOi) =
        object
        [ "$ref" .= refHistoricalOi
        , "$schema" .= schemaHistoricalOi
        , "definitions" .= definitionsHistoricalOi
        ]

instance FromJSON HistoricalOi where
    parseJSON (Object v) = HistoricalOi
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions candleDefinitions definitionsDataDefinitions historicalOiDefinitions) =
        object
        [ "Candle" .= candleDefinitions
        , "Data" .= definitionsDataDefinitions
        , "HistoricalOi" .= historicalOiDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Candle"
        <*> v .: "Data"
        <*> v .: "HistoricalOi"

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

instance ToJSON HistoricalOiClass where
    toJSON (HistoricalOiClass additionalPropertiesHistoricalOiClass propertiesHistoricalOiClass requiredHistoricalOiClass titleHistoricalOiClass historicalOiClassTypeHistoricalOiClass) =
        object
        [ "additionalProperties" .= additionalPropertiesHistoricalOiClass
        , "properties" .= propertiesHistoricalOiClass
        , "required" .= requiredHistoricalOiClass
        , "title" .= titleHistoricalOiClass
        , "type" .= historicalOiClassTypeHistoricalOiClass
        ]

instance FromJSON HistoricalOiClass where
    parseJSON (Object v) = HistoricalOiClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON HistoricalOiProperties where
    toJSON (HistoricalOiProperties historicalOiPropertiesDataHistoricalOiProperties statusHistoricalOiProperties) =
        object
        [ "data" .= historicalOiPropertiesDataHistoricalOiProperties
        , "status" .= statusHistoricalOiProperties
        ]

instance FromJSON HistoricalOiProperties where
    parseJSON (Object v) = HistoricalOiProperties
        <$> v .: "data"
        <*> v .: "status"
