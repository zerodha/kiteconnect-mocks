{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( OrderCancel (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , OrderID (..)
    , OrderCancelClass (..)
    , OrderCancelProperties (..)
    , DataClass (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data OrderCancel = OrderCancel
    { refOrderCancel :: Text
    , schemaOrderCancel :: Text
    , definitionsOrderCancel :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , orderCancelDefinitions :: OrderCancelClass
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

data OrderCancelClass = OrderCancelClass
    { additionalPropertiesOrderCancelClass :: Bool
    , propertiesOrderCancelClass :: OrderCancelProperties
    , requiredOrderCancelClass :: Vector Text
    , titleOrderCancelClass :: Text
    , orderCancelClassTypeOrderCancelClass :: Text
    } deriving (Show)

data OrderCancelProperties = OrderCancelProperties
    { orderCancelPropertiesDataOrderCancelProperties :: DataClass
    , statusOrderCancelProperties :: OrderID
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe OrderCancel
decodeTopLevel = decode

instance ToJSON OrderCancel where
    toJSON (OrderCancel refOrderCancel schemaOrderCancel definitionsOrderCancel) =
        object
        [ "$ref" .= refOrderCancel
        , "$schema" .= schemaOrderCancel
        , "definitions" .= definitionsOrderCancel
        ]

instance FromJSON OrderCancel where
    parseJSON (Object v) = OrderCancel
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions orderCancelDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "OrderCancel" .= orderCancelDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "OrderCancel"

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

instance ToJSON OrderCancelClass where
    toJSON (OrderCancelClass additionalPropertiesOrderCancelClass propertiesOrderCancelClass requiredOrderCancelClass titleOrderCancelClass orderCancelClassTypeOrderCancelClass) =
        object
        [ "additionalProperties" .= additionalPropertiesOrderCancelClass
        , "properties" .= propertiesOrderCancelClass
        , "required" .= requiredOrderCancelClass
        , "title" .= titleOrderCancelClass
        , "type" .= orderCancelClassTypeOrderCancelClass
        ]

instance FromJSON OrderCancelClass where
    parseJSON (Object v) = OrderCancelClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON OrderCancelProperties where
    toJSON (OrderCancelProperties orderCancelPropertiesDataOrderCancelProperties statusOrderCancelProperties) =
        object
        [ "data" .= orderCancelPropertiesDataOrderCancelProperties
        , "status" .= statusOrderCancelProperties
        ]

instance FromJSON OrderCancelProperties where
    parseJSON (Object v) = OrderCancelProperties
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
