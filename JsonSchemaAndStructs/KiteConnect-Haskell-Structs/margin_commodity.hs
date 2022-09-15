{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( MarginCommodity (..)
    , Definitions (..)
    , Available (..)
    , AvailableProperties (..)
    , AdhocMargin (..)
    , Data (..)
    , DataProperties (..)
    , AvailableClass (..)
    , Utilised (..)
    , MarginCommodityClass (..)
    , MarginCommodityProperties (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data MarginCommodity = MarginCommodity
    { refMarginCommodity :: Text
    , schemaMarginCommodity :: Text
    , definitionsMarginCommodity :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { availableDefinitions :: Available
    , definitionsDataDefinitions :: Data
    , marginCommodityDefinitions :: MarginCommodityClass
    } deriving (Show)

data Available = Available
    { additionalPropertiesAvailable :: Bool
    , propertiesAvailable :: AvailableProperties
    , requiredAvailable :: Vector Text
    , titleAvailable :: Text
    , availableTypeAvailable :: Text
    } deriving (Show)

data AvailableProperties = AvailableProperties
    { adhocMarginAvailableProperties :: AdhocMargin
    , cashAvailableProperties :: AdhocMargin
    , collateralAvailableProperties :: AdhocMargin
    , intradayPayinAvailableProperties :: AdhocMargin
    , liveBalanceAvailableProperties :: AdhocMargin
    , openingBalanceAvailableProperties :: AdhocMargin
    } deriving (Show)

data AdhocMargin = AdhocMargin
    { adhocMarginTypeAdhocMargin :: Text
    } deriving (Show)

data Data = Data
    { additionalPropertiesData :: Bool
    , propertiesData :: DataProperties
    , requiredData :: Vector Text
    , titleData :: Text
    , dataTypeData :: Text
    } deriving (Show)

data DataProperties = DataProperties
    { availableDataProperties :: AvailableClass
    , enabledDataProperties :: AdhocMargin
    , netDataProperties :: AdhocMargin
    , utilisedDataProperties :: Utilised
    } deriving (Show)

data AvailableClass = AvailableClass
    { refAvailableClass :: Text
    } deriving (Show)

data Utilised = Utilised
    { additionalPropertiesUtilised :: AdhocMargin
    , utilisedTypeUtilised :: Text
    } deriving (Show)

data MarginCommodityClass = MarginCommodityClass
    { additionalPropertiesMarginCommodityClass :: Bool
    , propertiesMarginCommodityClass :: MarginCommodityProperties
    , requiredMarginCommodityClass :: Vector Text
    , titleMarginCommodityClass :: Text
    , marginCommodityClassTypeMarginCommodityClass :: Text
    } deriving (Show)

data MarginCommodityProperties = MarginCommodityProperties
    { marginCommodityPropertiesDataMarginCommodityProperties :: AvailableClass
    , statusMarginCommodityProperties :: AdhocMargin
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MarginCommodity
decodeTopLevel = decode

instance ToJSON MarginCommodity where
    toJSON (MarginCommodity refMarginCommodity schemaMarginCommodity definitionsMarginCommodity) =
        object
        [ "$ref" .= refMarginCommodity
        , "$schema" .= schemaMarginCommodity
        , "definitions" .= definitionsMarginCommodity
        ]

instance FromJSON MarginCommodity where
    parseJSON (Object v) = MarginCommodity
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions availableDefinitions definitionsDataDefinitions marginCommodityDefinitions) =
        object
        [ "Available" .= availableDefinitions
        , "Data" .= definitionsDataDefinitions
        , "MarginCommodity" .= marginCommodityDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Available"
        <*> v .: "Data"
        <*> v .: "MarginCommodity"

instance ToJSON Available where
    toJSON (Available additionalPropertiesAvailable propertiesAvailable requiredAvailable titleAvailable availableTypeAvailable) =
        object
        [ "additionalProperties" .= additionalPropertiesAvailable
        , "properties" .= propertiesAvailable
        , "required" .= requiredAvailable
        , "title" .= titleAvailable
        , "type" .= availableTypeAvailable
        ]

instance FromJSON Available where
    parseJSON (Object v) = Available
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON AvailableProperties where
    toJSON (AvailableProperties adhocMarginAvailableProperties cashAvailableProperties collateralAvailableProperties intradayPayinAvailableProperties liveBalanceAvailableProperties openingBalanceAvailableProperties) =
        object
        [ "adhoc_margin" .= adhocMarginAvailableProperties
        , "cash" .= cashAvailableProperties
        , "collateral" .= collateralAvailableProperties
        , "intraday_payin" .= intradayPayinAvailableProperties
        , "live_balance" .= liveBalanceAvailableProperties
        , "opening_balance" .= openingBalanceAvailableProperties
        ]

instance FromJSON AvailableProperties where
    parseJSON (Object v) = AvailableProperties
        <$> v .: "adhoc_margin"
        <*> v .: "cash"
        <*> v .: "collateral"
        <*> v .: "intraday_payin"
        <*> v .: "live_balance"
        <*> v .: "opening_balance"

instance ToJSON AdhocMargin where
    toJSON (AdhocMargin adhocMarginTypeAdhocMargin) =
        object
        [ "type" .= adhocMarginTypeAdhocMargin
        ]

instance FromJSON AdhocMargin where
    parseJSON (Object v) = AdhocMargin
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
    toJSON (DataProperties availableDataProperties enabledDataProperties netDataProperties utilisedDataProperties) =
        object
        [ "available" .= availableDataProperties
        , "enabled" .= enabledDataProperties
        , "net" .= netDataProperties
        , "utilised" .= utilisedDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "available"
        <*> v .: "enabled"
        <*> v .: "net"
        <*> v .: "utilised"

instance ToJSON AvailableClass where
    toJSON (AvailableClass refAvailableClass) =
        object
        [ "$ref" .= refAvailableClass
        ]

instance FromJSON AvailableClass where
    parseJSON (Object v) = AvailableClass
        <$> v .: "$ref"

instance ToJSON Utilised where
    toJSON (Utilised additionalPropertiesUtilised utilisedTypeUtilised) =
        object
        [ "additionalProperties" .= additionalPropertiesUtilised
        , "type" .= utilisedTypeUtilised
        ]

instance FromJSON Utilised where
    parseJSON (Object v) = Utilised
        <$> v .: "additionalProperties"
        <*> v .: "type"

instance ToJSON MarginCommodityClass where
    toJSON (MarginCommodityClass additionalPropertiesMarginCommodityClass propertiesMarginCommodityClass requiredMarginCommodityClass titleMarginCommodityClass marginCommodityClassTypeMarginCommodityClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMarginCommodityClass
        , "properties" .= propertiesMarginCommodityClass
        , "required" .= requiredMarginCommodityClass
        , "title" .= titleMarginCommodityClass
        , "type" .= marginCommodityClassTypeMarginCommodityClass
        ]

instance FromJSON MarginCommodityClass where
    parseJSON (Object v) = MarginCommodityClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MarginCommodityProperties where
    toJSON (MarginCommodityProperties marginCommodityPropertiesDataMarginCommodityProperties statusMarginCommodityProperties) =
        object
        [ "data" .= marginCommodityPropertiesDataMarginCommodityProperties
        , "status" .= statusMarginCommodityProperties
        ]

instance FromJSON MarginCommodityProperties where
    parseJSON (Object v) = MarginCommodityProperties
        <$> v .: "data"
        <*> v .: "status"
