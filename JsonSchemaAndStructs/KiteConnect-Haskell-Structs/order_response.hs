{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( OrderResponse (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , OrderID (..)
    , OrderResponseClass (..)
    , OrderResponseProperties (..)
    , DataClass (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data OrderResponse = OrderResponse
    { refOrderResponse :: Text
    , schemaOrderResponse :: Text
    , definitionsOrderResponse :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , orderResponseDefinitions :: OrderResponseClass
    } deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { orderIDDataProperties :: OrderID
    } deriving (Show)

data OrderID = OrderID
    { orderIDTypeOrderID :: Text
    } deriving (Show)

data OrderResponseClass = OrderResponseClass
    { additionalPropertiesOrderResponseClass :: Bool
    , propertiesOrderResponseClass :: OrderResponseProperties
    , requiredOrderResponseClass :: Vector Text
    , titleOrderResponseClass :: Text
    , orderResponseClassTypeOrderResponseClass :: Text
    } deriving (Show)

data OrderResponseProperties = OrderResponseProperties
    { orderResponsePropertiesDataOrderResponseProperties :: DataClass
    , statusOrderResponseProperties :: OrderID
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe OrderResponse
decodeTopLevel = decode

instance ToJSON OrderResponse where
    toJSON (OrderResponse refOrderResponse schemaOrderResponse definitionsOrderResponse) =
        object
        [ "$ref" .= refOrderResponse
        , "$schema" .= schemaOrderResponse
        , "definitions" .= definitionsOrderResponse
        ]

instance FromJSON OrderResponse where
    parseJSON (Object v) = OrderResponse
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions orderResponseDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "OrderResponse" .= orderResponseDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "OrderResponse"

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
    toJSON (DataProperties orderIDDataProperties) =
        object
        [ "order_id" .= orderIDDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "order_id"

instance ToJSON OrderID where
    toJSON (OrderID orderIDTypeOrderID) =
        object
        [ "type" .= orderIDTypeOrderID
        ]

instance FromJSON OrderID where
    parseJSON (Object v) = OrderID
        <$> v .: "type"

instance ToJSON OrderResponseClass where
    toJSON (OrderResponseClass additionalPropertiesOrderResponseClass propertiesOrderResponseClass requiredOrderResponseClass titleOrderResponseClass orderResponseClassTypeOrderResponseClass) =
        object
        [ "additionalProperties" .= additionalPropertiesOrderResponseClass
        , "properties" .= propertiesOrderResponseClass
        , "required" .= requiredOrderResponseClass
        , "title" .= titleOrderResponseClass
        , "type" .= orderResponseClassTypeOrderResponseClass
        ]

instance FromJSON OrderResponseClass where
    parseJSON (Object v) = OrderResponseClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON OrderResponseProperties where
    toJSON (OrderResponseProperties orderResponsePropertiesDataOrderResponseProperties statusOrderResponseProperties) =
        object
        [ "data" .= orderResponsePropertiesDataOrderResponseProperties
        , "status" .= statusOrderResponseProperties
        ]

instance FromJSON OrderResponseProperties where
    parseJSON (Object v) = OrderResponseProperties
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
