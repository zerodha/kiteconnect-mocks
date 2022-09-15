{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( Margins (..)
    , Definitions (..)
    , Available (..)
    , AvailableProperties (..)
    , AdhocMargin (..)
    , Data (..)
    , DataProperties (..)
    , Commodity (..)
    , Ity (..)
    , ItyProperties (..)
    , Utilised (..)
    , MarginsClass (..)
    , MarginsProperties (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

data Margins = Margins
    { refMargins :: Text
    , schemaMargins :: Text
    , definitionsMargins :: Definitions
    } deriving (Show)

data Definitions = Definitions
    { availableDefinitions :: Available
    , definitionsDataDefinitions :: Data
    , ityDefinitions :: Ity
    , marginsDefinitions :: MarginsClass
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
    { commodityDataProperties :: Commodity
    , equityDataProperties :: Commodity
    } deriving (Show)

data Commodity = Commodity
    { refCommodity :: Text
    } deriving (Show)

data Ity = Ity
    { additionalPropertiesIty :: Bool
    , propertiesIty :: ItyProperties
    , requiredIty :: Vector Text
    , titleIty :: Text
    , ityTypeIty :: Text
    } deriving (Show)

data ItyProperties = ItyProperties
    { availableItyProperties :: Commodity
    , enabledItyProperties :: AdhocMargin
    , netItyProperties :: AdhocMargin
    , utilisedItyProperties :: Utilised
    } deriving (Show)

data Utilised = Utilised
    { additionalPropertiesUtilised :: AdhocMargin
    , utilisedTypeUtilised :: Text
    } deriving (Show)

data MarginsClass = MarginsClass
    { additionalPropertiesMarginsClass :: Bool
    , propertiesMarginsClass :: MarginsProperties
    , requiredMarginsClass :: Vector Text
    , titleMarginsClass :: Text
    , marginsClassTypeMarginsClass :: Text
    } deriving (Show)

data MarginsProperties = MarginsProperties
    { marginsPropertiesDataMarginsProperties :: Commodity
    , statusMarginsProperties :: AdhocMargin
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe Margins
decodeTopLevel = decode

instance ToJSON Margins where
    toJSON (Margins refMargins schemaMargins definitionsMargins) =
        object
        [ "$ref" .= refMargins
        , "$schema" .= schemaMargins
        , "definitions" .= definitionsMargins
        ]

instance FromJSON Margins where
    parseJSON (Object v) = Margins
        <$> v .: "$ref"
        <*> v .: "$schema"
        <*> v .: "definitions"

instance ToJSON Definitions where
    toJSON (Definitions availableDefinitions definitionsDataDefinitions ityDefinitions marginsDefinitions) =
        object
        [ "Available" .= availableDefinitions
        , "Data" .= definitionsDataDefinitions
        , "Ity" .= ityDefinitions
        , "Margins" .= marginsDefinitions
        ]

instance FromJSON Definitions where
    parseJSON (Object v) = Definitions
        <$> v .: "Available"
        <*> v .: "Data"
        <*> v .: "Ity"
        <*> v .: "Margins"

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
    toJSON (DataProperties commodityDataProperties equityDataProperties) =
        object
        [ "commodity" .= commodityDataProperties
        , "equity" .= equityDataProperties
        ]

instance FromJSON DataProperties where
    parseJSON (Object v) = DataProperties
        <$> v .: "commodity"
        <*> v .: "equity"

instance ToJSON Commodity where
    toJSON (Commodity refCommodity) =
        object
        [ "$ref" .= refCommodity
        ]

instance FromJSON Commodity where
    parseJSON (Object v) = Commodity
        <$> v .: "$ref"

instance ToJSON Ity where
    toJSON (Ity additionalPropertiesIty propertiesIty requiredIty titleIty ityTypeIty) =
        object
        [ "additionalProperties" .= additionalPropertiesIty
        , "properties" .= propertiesIty
        , "required" .= requiredIty
        , "title" .= titleIty
        , "type" .= ityTypeIty
        ]

instance FromJSON Ity where
    parseJSON (Object v) = Ity
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON ItyProperties where
    toJSON (ItyProperties availableItyProperties enabledItyProperties netItyProperties utilisedItyProperties) =
        object
        [ "available" .= availableItyProperties
        , "enabled" .= enabledItyProperties
        , "net" .= netItyProperties
        , "utilised" .= utilisedItyProperties
        ]

instance FromJSON ItyProperties where
    parseJSON (Object v) = ItyProperties
        <$> v .: "available"
        <*> v .: "enabled"
        <*> v .: "net"
        <*> v .: "utilised"

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

instance ToJSON MarginsClass where
    toJSON (MarginsClass additionalPropertiesMarginsClass propertiesMarginsClass requiredMarginsClass titleMarginsClass marginsClassTypeMarginsClass) =
        object
        [ "additionalProperties" .= additionalPropertiesMarginsClass
        , "properties" .= propertiesMarginsClass
        , "required" .= requiredMarginsClass
        , "title" .= titleMarginsClass
        , "type" .= marginsClassTypeMarginsClass
        ]

instance FromJSON MarginsClass where
    parseJSON (Object v) = MarginsClass
        <$> v .: "additionalProperties"
        <*> v .: "properties"
        <*> v .: "required"
        <*> v .: "title"
        <*> v .: "type"

instance ToJSON MarginsProperties where
    toJSON (MarginsProperties marginsPropertiesDataMarginsProperties statusMarginsProperties) =
        object
        [ "data" .= marginsPropertiesDataMarginsProperties
        , "status" .= statusMarginsProperties
        ]

instance FromJSON MarginsProperties where
    parseJSON (Object v) = MarginsProperties
        <$> v .: "data"
        <*> v .: "status"
