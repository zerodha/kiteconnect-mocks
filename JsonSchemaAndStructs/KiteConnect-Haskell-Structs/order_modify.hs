{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( OrderModify (..)
    , Definitions (..)
    , Data (..)
    , DataProperties (..)
    , OrderID (..)
    , OrderModifyClass (..)
    , OrderModifyProperties (..)
    , DataClass (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data OrderModify = OrderModify
    { refOrderModify :: Text
    , schemaOrderModify :: Text
    , definitionsOrderModify :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { definitionsDataDefinitions :: Data
    , orderModifyDefinitions :: OrderModifyClass
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

data OrderModifyClass = OrderModifyClass
    { additionalPropertiesOrderModifyClass :: Bool
    , propertiesOrderModifyClass :: OrderModifyProperties
    , requiredOrderModifyClass :: Vector Text
    , titleOrderModifyClass :: Text
    , orderModifyClassTypeOrderModifyClass :: Text
    } deriving (Show)

data OrderModifyProperties = OrderModifyProperties
    { orderModifyPropertiesDataOrderModifyProperties :: DataClass
    , statusOrderModifyProperties :: OrderID
    } deriving (Show)

data DataClass = DataClass
    { refDataClass :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe OrderModify
decodeTopLevel = decode

instance ToJSON OrderModify where
    toJSON (OrderModify refOrderModify schemaOrderModify definitionsOrderModify) =
        object
        [ "$ref" .= refOrderModify
        , "$schema" .= schemaOrderModify
        , "definitions" .= definitionsOrderModify
        ]

instance FromJSON OrderModify where
    parseJSON (Object v) = OrderModify
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions definitionsDataDefinitions orderModifyDefinitions) =
        object
        [ "Data" .= definitionsDataDefinitions
        , "OrderModify" .= orderModifyDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Data"
        <*> v .: "OrderModify"

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

instance ToJSON OrderModifyClass where
    toJSON (OrderModifyClass additionalPropertiesOrderModifyClass propertiesOrderModifyClass requiredOrderModifyClass titleOrderModifyClass orderModifyClassTypeOrderModifyClass) =
        object
        [ "additionalProperties" .= additionalPropertiesOrderModifyClass
        , "properties" .= propertiesOrderModifyClass
        , "required" .= requiredOrderModifyClass
        , "title" .= titleOrderModifyClass
        , "type" .= orderModifyClassTypeOrderModifyClass
        ]

instance FromJSON OrderModifyClass where
    parseJSON (Object v) = OrderModifyClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON OrderModifyProperties where
    toJSON (OrderModifyProperties orderModifyPropertiesDataOrderModifyProperties statusOrderModifyProperties) =
        object
        [ "data" .= orderModifyPropertiesDataOrderModifyProperties
        , "status" .= statusOrderModifyProperties
        ]

instance FromJSON OrderModifyProperties where
    parseJSON (Object v) = OrderModifyProperties
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
